#!/usr/bin/env python3
"""Serve the offline tropical viewer and its exact Haskell geometry backend."""
import argparse
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import mimetypes
from pathlib import Path
import re
import shutil
import subprocess
import threading
from urllib.parse import urlsplit, unquote

MAX_BODY = 32768
MAX_TERMS = 32
RATIONAL = re.compile(r"-?[0-9]{1,18}(?:/[1-9][0-9]{0,17})?\Z")
ASSETS = {
    "/": "index.html", "/index.html": "index.html",
    "/slices.html": "slices.html", "/slices.js": "slices.js",
    "/slices.css": "slices.css", "/app.js": "app.js",
    "/style.css": "style.css",
    "/examples/genus-1-cubic.json": "examples/genus-1-cubic.json",
}


def validate_request(value, slice_request=False):
    expected = {"terms", "height"} if slice_request else {"terms"}
    if not isinstance(value, dict) or set(value) != expected:
        if slice_request:
            raise ValueError('Expected an object containing "terms" and "height".')
        raise ValueError('Expected an object containing "terms".')
    if slice_request and (not isinstance(value["height"], str) or not RATIONAL.fullmatch(value["height"])):
        raise ValueError("Height must be an integer or fraction string, with at most 18 digits per part and a positive denominator.")
    terms = value["terms"]
    if not isinstance(terms, list) or not 1 <= len(terms) <= MAX_TERMS:
        raise ValueError(f"Provide between 1 and {MAX_TERMS} terms.")
    required = {"x", "y", "z", "coefficient"} if slice_request else {"x", "y", "coefficient"}
    exponents = ("x", "y", "z") if slice_request else ("x", "y")
    for term in terms:
        if not isinstance(term, dict) or set(term) != required:
            raise ValueError("Each slice term needs x, y, z, and coefficient." if slice_request else "Each term needs x, y, and coefficient.")
        if any(type(term[k]) is not int or abs(term[k]) > 100 for k in exponents):
            raise ValueError("Exponents must be integers between -100 and 100.")
        if not isinstance(term["coefficient"], str) or not RATIONAL.fullmatch(term["coefficient"]):
            raise ValueError("Coefficients must be integer or fraction strings, with at most 18 digits per part and a positive denominator.")
    return value


class ViewerServer(ThreadingHTTPServer):
    daemon_threads = True
    def __init__(self, address, assets, backend, timeout=15):
        super().__init__(address, ViewerHandler)
        self.assets = Path(assets).resolve()
        self.backend = str(backend)
        self.backend_timeout = timeout
        self.backend_lock = threading.BoundedSemaphore(2)


class ViewerHandler(BaseHTTPRequestHandler):
    def setup(self):
        super().setup()
        self.connection.settimeout(10)

    def send_bytes(self, status, data, content_type):
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(data)))
        self.send_header("X-Content-Type-Options", "nosniff")
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(data)

    def json_response(self, status, value):
        self.send_bytes(status, json.dumps(value).encode(), "application/json; charset=utf-8")

    def trusted_request(self):
        # Reject DNS rebinding and cross-origin requests even on loopback.
        port = self.server.server_address[1]
        allowed = {f"127.0.0.1:{port}", f"localhost:{port}"}
        host = self.headers.get("Host", "")
        origin = self.headers.get("Origin")
        return host in allowed and (origin is None or origin == "http://" + host)

    def do_GET(self):
        if not self.trusted_request():
            self.json_response(403, {"error": "Use the local viewer address."})
            return
        path = unquote(urlsplit(self.path).path)
        name = ASSETS.get(path)
        if path.startswith("/vendor/"):
            candidate = Path(path[1:])
            if all(part not in (".", "..") and not part.startswith(".") for part in candidate.parts):
                name = str(candidate)
        if name is None:
            self.json_response(404, {"error": "Not found."})
            return
        target = (self.server.assets / name).resolve()
        if not target.is_relative_to(self.server.assets) or not target.is_file():
            self.json_response(404, {"error": "Not found."})
            return
        self.send_bytes(200, target.read_bytes(), mimetypes.guess_type(target.name)[0] or "application/octet-stream")

    def do_POST(self):
        if not self.trusted_request():
            self.json_response(403, {"error": "Cross-origin requests are not allowed."})
            return
        if self.path not in ("/api/curve", "/api/slice"):
            self.json_response(404, {"error": "Not found."})
            return
        is_slice = self.path == "/api/slice"
        if self.headers.get("Content-Type", "").split(";", 1)[0].strip() != "application/json":
            self.json_response(415, {"error": "Use application/json."})
            return
        try:
            length = int(self.headers.get("Content-Length", "0"))
            if not 0 < length <= MAX_BODY:
                raise ValueError("Request body is empty or too large.")
            request = validate_request(json.loads(self.rfile.read(length)), is_slice)
        except (ValueError, UnicodeError):
            self.json_response(400, {"error": "Invalid request: provide 1–32 terms, integer exponents within ±100, and integer/fraction coefficient and height strings (18 digits per part)." if is_slice else "Invalid request: provide 1–32 terms, integer exponents within ±100, and integer/fraction coefficient strings (18 digits per part)."})
            return
        if not self.server.backend_lock.acquire(blocking=False):
            self.json_response(503, {"error": "Geometry backend is busy; try again."})
            return
        try:
            result = subprocess.run([self.server.backend], input=json.dumps(request), text=True,
                                    capture_output=True, timeout=self.server.backend_timeout, check=False)
            try:
                value = json.loads(result.stdout)
            except ValueError:
                self.json_response(502, {"error": "Geometry backend returned an invalid response."})
                return
            if not isinstance(value, dict):
                self.json_response(502, {"error": "Geometry backend returned an invalid response."})
            elif "error" in value:
                self.json_response(422, {"error": str(value["error"])})
            elif result.returncode:
                self.json_response(502, {"error": "Geometry backend failed."})
            else:
                self.json_response(200, value)
        except subprocess.TimeoutExpired:
            self.json_response(504, {"error": "Geometry computation timed out. Try fewer terms."})
        except OSError:
            self.json_response(502, {"error": "Cannot start the geometry backend."})
        finally:
            self.server.backend_lock.release()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--backend", type=Path, help="Path to tropical-viewer-geometry (default: Stack install location)")
    parser.add_argument("--port", type=int, default=8765)
    args = parser.parse_args()
    if not 1 <= args.port <= 65535:
        parser.error("port must be between 1 and 65535")
    root = Path(__file__).resolve().parent
    backend = args.backend
    if backend is None:
        stack = shutil.which("stack") or str(Path.home() / ".local/bin/stack")
        try:
            install = subprocess.check_output([stack, "path", "--local-install-root"], cwd=root.parent, text=True, timeout=60).strip()
            backend = Path(install) / "bin/tropical-viewer-geometry"
        except (OSError, subprocess.SubprocessError):
            parser.error("Cannot find Stack install location; supply --backend PATH.")
    backend = backend.expanduser().resolve()
    if not backend.is_file():
        parser.error(f"Backend not found: {backend}. Run stack build --copy-bins --local-bin-path PATH and supply --backend PATH/tropical-viewer-geometry.")
    server = ViewerServer(("127.0.0.1", args.port), root, backend)
    print(f"Tropical viewer: http://127.0.0.1:{args.port}", flush=True)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()


if __name__ == "__main__":
    main()
