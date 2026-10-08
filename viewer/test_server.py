"""HTTP contract checks; run python3 -m unittest discover -s viewer -p 'test_*.py'."""
import http.client
import json
from pathlib import Path
import tempfile
import threading
import unittest
from unittest.mock import patch
import subprocess

from serve import ViewerServer, backend_request, validate_request

TERM = {"x": 0, "y": 0, "coefficient": "-1/2"}
TERM3 = {"x": 0, "y": 0, "z": 1, "coefficient": "-1/2"}


class RequestValidation(unittest.TestCase):
    def test_exact_fraction(self):
        self.assertEqual(validate_request({"terms": [TERM]}), {"terms": [TERM]})

    def test_named_methods_and_graph3_requests(self):
        for method in ("direct", "hull", "lrs"):
            self.assertEqual(validate_request({"terms": [TERM], "method": method}), {"terms": [TERM], "method": method})
            self.assertEqual(validate_request({"terms": [TERM3], "method": method}, "graph3"), {"terms": [TERM3], "method": method})
        self.assertEqual(validate_request({"terms": [TERM3]}, "graph3"), {"terms": [TERM3]})
        self.assertEqual(backend_request({"terms": [TERM3], "method": "lrs"}, "graph3"), {"terms": [TERM3], "method": "lrs", "kind": "graph3"})
        self.assertEqual(backend_request({"terms": [TERM], "method": "hull"}, "curve"), {"terms": [TERM], "method": "hull"})
        self.assertEqual(backend_request({"terms": [TERM3], "height": "1"}, "slice"), {"terms": [TERM3], "height": "1"})
        invalid = [({"terms": [TERM], "method": "other"}, "curve"), ({"terms": [TERM], "method": 1}, "curve"),
                   ({"terms": [TERM], "kind": "graph3"}, "curve"), ({"terms": [TERM3]}, "curve"),
                   ({"terms": [TERM]}, "graph3"), ({"terms": [TERM3], "height": "0"}, "graph3"),
                   ({"terms": [TERM3], "method": "nope"}, "graph3"), ({"terms": [TERM3], "height": "0", "method": "lrs"}, "slice"),
                   ({"terms": [TERM3]}, "other")]
        for value, kind in invalid:
            with self.subTest(value=value, kind=kind), self.assertRaises(ValueError):
                validate_request(value, kind)

    def test_invalid_values(self):
        invalid = [[], {}, {"terms": []}, {"terms": [TERM] * 33},
                   {"terms": [dict(TERM, x=True)]}, {"terms": [dict(TERM, y=101)]},
                   {"terms": [dict(TERM, x=1.5)]},
                   {"terms": [dict(TERM, coefficient="١")]},
                   {"terms": [dict(TERM, coefficient="1/02")]},
                   {"terms": [dict(TERM, coefficient="1/0")]},
                   {"terms": [dict(TERM, coefficient="1.5")]},
                   {"terms": [dict(TERM, coefficient="1" * 19)]},
                   {"terms": [dict(TERM, coefficient=1)]}]
        for value in invalid:
            with self.subTest(value=value), self.assertRaises(ValueError):
                validate_request(value)


class ServiceContract(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temp = tempfile.TemporaryDirectory()
        assets = Path(cls.temp.name)
        (assets / "index.html").write_text("viewer")
        (assets / "serve.py").write_text("private source")
        (assets / "vendor").mkdir()
        (assets / "vendor" / "library.js").write_text("asset")
        cls.server = ViewerServer(("127.0.0.1", 0), assets, "/unused/backend", timeout=.01)
        cls.thread = threading.Thread(target=cls.server.serve_forever, daemon=True)
        cls.thread.start()
        cls.port = cls.server.server_address[1]

    @classmethod
    def tearDownClass(cls):
        cls.server.shutdown()
        cls.server.server_close()
        cls.thread.join()
        cls.temp.cleanup()

    def request(self, method="POST", path="/api/curve", body=None, headers=None):
        conn = http.client.HTTPConnection("127.0.0.1", self.port, timeout=2)
        data = json.dumps({"terms": [TERM]}) if body is None else body
        h = {"Content-Type": "application/json"}
        h.update(headers or {})
        conn.request(method, path, body=data if method == "POST" else None, headers=h)
        response = conn.getresponse()
        status, content = response.status, response.read()
        conn.close()
        return status, content

    @patch("serve.subprocess.run")
    def test_success_exact_payload(self, run):
        curve = {"vertices": [{"point": ["1/3", "-2/5"]}]}
        run.return_value = subprocess.CompletedProcess([], 0, json.dumps(curve), "")
        status, content = self.request()
        self.assertEqual(status, 200)
        self.assertEqual(json.loads(content), curve)
        self.assertEqual(json.loads(run.call_args.kwargs["input"]), {"terms": [TERM]})
        self.assertEqual(run.call_args.args[0], ["/unused/backend"])

    @patch("serve.subprocess.run")
    def test_graph3_and_method_requests_reach_the_backend_as_named(self, run):
        graph = {"kind": "graph3", "method": "lrs", "vertices": []}
        run.return_value = subprocess.CompletedProcess([], 0, json.dumps(graph), "")
        status, content = self.request(path="/api/graph3", body=json.dumps({"terms": [TERM3], "method": "lrs"}))
        self.assertEqual(status, 200)
        self.assertEqual(json.loads(content), graph)
        self.assertEqual(json.loads(run.call_args.kwargs["input"]), {"terms": [TERM3], "method": "lrs", "kind": "graph3"})
        run.return_value = subprocess.CompletedProcess([], 0, '{"method":"hull","vertices":[]}', "")
        self.assertEqual(self.request(body=json.dumps({"terms": [TERM], "method": "hull"}))[0], 200)
        self.assertEqual(json.loads(run.call_args.kwargs["input"]), {"terms": [TERM], "method": "hull"})
        run.reset_mock()
        for path, body in [("/api/graph3", {"terms": [TERM3], "method": "other"}), ("/api/graph3", {"terms": [TERM]}),
                           ("/api/curve", {"terms": [TERM], "method": "other"}), ("/api/slice", {"terms": [TERM3], "height": "0", "method": "lrs"})]:
            with self.subTest(path=path, body=body):
                status, content = self.request(path=path, body=json.dumps(body))
                self.assertEqual(status, 400)
                self.assertIn("direct, hull, or lrs" if path != "/api/slice" else "height", json.loads(content)["error"])
        run.assert_not_called()

    @patch("serve.subprocess.run")
    def test_backend_errors(self, run):
        for result, expected in [(subprocess.CompletedProcess([], 0, '{"error":"invalid geometry"}', ''), 422),
                                 (subprocess.CompletedProcess([], 1, '{}', ''), 502),
                                 (subprocess.CompletedProcess([], 0, 'not json', ''), 502),
                                 (subprocess.CompletedProcess([], 0, '[]', ''), 502)]:
            run.return_value = result
            self.assertEqual(self.request()[0], expected)

    @patch("serve.subprocess.run", side_effect=subprocess.TimeoutExpired("backend", .01))
    def test_timeout(self, run):
        self.assertEqual(self.request()[0], 504)

    @patch("serve.subprocess.run", side_effect=FileNotFoundError())
    def test_missing_backend(self, run):
        self.assertEqual(self.request()[0], 502)

    @patch("serve.subprocess.run")
    def test_bad_requests_never_run_backend(self, run):
        self.assertEqual(self.request(body="{")[0], 400)
        self.assertEqual(self.request(body="x" * 32769)[0], 400)
        self.assertEqual(self.request(headers={"Content-Type": "text/plain"})[0], 415)
        self.assertEqual(self.request(headers={"Origin": "https://evil.example"})[0], 403)
        self.assertEqual(self.request(headers={"Host": f"evil.example:{self.port}"})[0], 403)
        self.assertEqual(self.request(path="/api/other")[0], 404)
        run.assert_not_called()

    def test_static_scope(self):
        self.assertEqual(self.request("GET", "/")[1], b"viewer")
        self.assertEqual(self.request("GET", "/vendor/library.js")[1], b"asset")
        for path in ("/serve.py", "/../README.md", "/vendor/../serve.py", "/vendor/%2e%2e/serve.py"):
            with self.subTest(path=path):
                self.assertEqual(self.request("GET", path)[0], 404)

    def test_busy_backend(self):
        self.server.backend_lock.acquire()
        self.server.backend_lock.acquire()
        try:
            self.assertEqual(self.request()[0], 503)
        finally:
            self.server.backend_lock.release()
            self.server.backend_lock.release()


if __name__ == "__main__":
    unittest.main()
