#!/usr/bin/env python3
"""Run reproducible exact differential checks, then serialized benchmarks.

The Haskell worker parses each fresh JSON request before starting its own
monotonic wall/CPU timers, forces the complete result in the timed interval,
and serializes it after the timers stop. This driver keeps every result rather
than reducing repeated outputs to hashes.
"""
from __future__ import annotations

import argparse
from datetime import datetime, timezone
import fcntl
import hashlib
import json
import os
from pathlib import Path
import platform
import random
import select
import shlex
import shutil
import subprocess
import sys
import time


ROOT = Path(__file__).resolve().parents[1]
COMPARISON = Path(__file__).resolve().parent


def rational(value: int | str) -> str:
    return str(value)


def hull_case(case_id: str, points: list[list[int | str]], dim: int,
              methods: list[str] | None = None) -> dict:
    return {
        "case_id": case_id,
        "kind": f"hull{dim}",
        "points": [[rational(v) for v in point] for point in points],
        "methods": methods or ["tailored", "lrs"],
    }


def curve_case(case_id: str, terms: list[tuple[int, int, int | str]],
               methods: list[str] | None = None) -> dict:
    return {
        "case_id": case_id,
        "kind": "curve",
        "terms": [
            {"x": x, "y": y, "coefficient": rational(c)}
            for x, y, c in terms
        ],
        "methods": methods or ["exact", "tailored", "lrs"],
    }


def skeleton3_case(case_id: str, terms: list[tuple[int, int, int, int | str]],
                  methods: list[str] | None = None) -> dict:
    return {
        "case_id": case_id,
        "kind": "skeleton3",
        "terms": [
            {"x": x, "y": y, "z": z, "coefficient": rational(c)}
            for x, y, z, c in terms
        ],
        "methods": methods or ["exact", "original", "lrs"],
    }


def make_fixtures(seed: int) -> list[dict]:
    rng = random.Random(seed)
    fixtures: list[dict] = []

    fixtures += [
        hull_case("hull2-triangle", [[0, 0], [5, 0], [1, 4]], 2),
        hull_case("hull2-square", [[0, 0], [0, 4], [4, 0], [4, 4]], 2),
        hull_case("hull2-square-duplicates", [[0, 0], [0, 4], [4, 0], [4, 4], [0, 0], [2, 2]], 2),
        hull_case("hull2-fractional", [["0", "0"], ["1/2", "0"], ["0", "3/2"]], 2, ["lrs"]),
    ]

    def random_points(count: int, dimension: int, bound: int) -> list[list[int]]:
        point_set: set[tuple[int, ...]] = set()
        while len(point_set) < count:
            point_set.add(tuple(rng.randint(-bound, bound) for _ in range(dimension)))
        return [list(point) for point in sorted(point_set)]

    for count in (8, 16, 24, 32):
        fixtures.append(hull_case(f"hull2-seeded-n{count}", random_points(count, 2, 25), 2))
    fixtures.append(hull_case("hull2-collinear-vertical", [[0, 0], [0, 1], [0, 2], [0, 3]], 2))

    fixtures += [
        hull_case("hull3-tetrahedron", [[0, 0, 0], [5, 0, 0], [0, 5, 0], [0, 0, 5]], 3),
        hull_case("hull3-cube", [[x, y, z] for x in (0, 3) for y in (0, 3) for z in (0, 3)], 3),
        hull_case("hull3-cube-duplicates", [[x, y, z] for x in (0, 3) for y in (0, 3) for z in (0, 3)] + [[0, 0, 0]], 3),
        hull_case("hull3-fractional", [["0", "0", "0"], ["1/2", "0", "0"], ["0", "3/2", "0"], ["0", "0", "2/3"]], 3, ["lrs"]),
    ]
    for count in (8, 16, 24, 32):
        fixtures.append(hull_case(f"hull3-seeded-n{count}", random_points(count, 3, 18), 3))
    fixtures.append(hull_case("hull3-coplanar", [[x, y, 0] for x, y in ((0, 0), (1, 0), (1, 1), (0, 1))], 3))

    # This standard tropical line has integral dual vertices and is also sent
    # through the historical Hypersurface API for the 10x-ray adapter check.
    fixtures.append(curve_case("curve-min-linear", [(0, 0, 0), (1, 0, 0), (0, 1, 0)],
                               ["exact", "tailored", "lrs", "legacy"]))
    fixtures.append(curve_case("curve-flat-square", [(0, 0, 0), (2, 0, 0), (0, 2, 0), (2, 2, 0)],
                               ["exact", "tailored", "lrs", "legacy"]))
    fixtures.append(curve_case("curve-lifted-square", [(0, 0, 0), (2, 0, 0), (0, 2, 0), (2, 2, 1)]))
    fixtures.append(curve_case("curve-integral-lifted-square", [(0, 0, 0), (1, 0, 0), (0, 1, 0), (1, 1, 1)],
                               ["exact", "tailored", "lrs", "legacy"]))
    # Original benchmark f1 = 1*x^2 + x*y + 1*y^2 + x + y + 2.
    fixtures.append(curve_case("curve-benchmark-f1", [(2, 0, 1), (1, 1, 0), (0, 2, 1), (1, 0, 0), (0, 1, 0), (0, 0, 2)],
                               ["exact", "tailored", "lrs", "legacy"]))
    fixtures.append(curve_case("curve-cubic-lattice", [
        (i, j, i*i + i*j + j*j) for i in range(4) for j in range(4-i)
    ], ["exact", "tailored", "lrs", "legacy"]))
    fixtures.append(curve_case("curve-fractional-coefficient", [(0, 0, 0), (2, 0, "1/3"), (0, 2, 1), (2, 2, 0)]))
    for count in (8, 16, 32):
        exponents: set[tuple[int, int]] = {(0, 0), (7, 0), (0, 7)}
        while len(exponents) < count:
            exponents.add((rng.randint(0, 7), rng.randint(0, 7)))
        terms = [(x, y, rng.randint(-5, 8)) for x, y in sorted(exponents)]
        fixtures.append(curve_case(f"curve-seeded-n{count}", terms))

    # These are the graph one-skeletons of 3-variable tropical hypersurfaces,
    # not full 2D surfaces and not horizontal slice curves.
    simplex = [(0, 0, 0, 0), (1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0)]
    fixtures.append(skeleton3_case("skeleton3-simplex", simplex))
    # Historical generalized graph code compared facet normals by magnitude,
    # losing the bounded edge shared by these unequal-height tetrahedra.
    fixtures.append(skeleton3_case("skeleton3-unequal-cell-heights", [
        (0, 0, 1, 0), (1, 0, 1, 0), (0, 1, 1, 0),
        (0, 0, 0, 1), (0, 0, 3, 1),
    ]))
    fixtures.append(skeleton3_case("skeleton3-fractional-coefficient", simplex + [(1, 1, 1, "1/3")]))
    fixtures.append(skeleton3_case("skeleton3-fractional-vertex", [
        (0, 0, 0, 0), (2, 0, 0, -1), (0, 3, 0, -2), (0, 0, 4, -3)
    ]))
    fixtures.append(skeleton3_case("skeleton3-quadratic-lattice", [
        (i, j, k, i*i + j*j + k*k + i*j)
        for i in range(3) for j in range(3-i) for k in range(3-i-j)
    ]))
    fixtures.append(skeleton3_case("skeleton3-simplex-duplicate", simplex + [(1, 0, 0, -1)]))
    for count in (8, 16):
        exponents: set[tuple[int, int, int]] = {(0, 0, 0), (1, 0, 0), (0, 1, 0), (0, 0, 1)}
        while len(exponents) < count:
            exponents.add((rng.randint(0, 4), rng.randint(0, 4), rng.randint(0, 4)))
        terms = [(x, y, z, rng.randint(-4, 7)) for x, y, z in sorted(exponents)]
        fixtures.append(skeleton3_case(f"skeleton3-seeded-n{count}", terms))
    fixtures.append(skeleton3_case("skeleton3-coplanar-support", [
        (0, 0, 0, 0), (2, 0, 0, 0), (0, 2, 0, 1), (2, 2, 0, 0)
    ]))

    return fixtures


class Worker:
    """Persistent JSONL child with a per-request timeout and clean restart."""

    def __init__(self, command: list[str], timeout: float):
        self.command = command
        self.timeout = timeout
        self.proc: subprocess.Popen | None = None
        self.pending = bytearray()
        self.start()

    def start(self) -> None:
        self.close()
        self.proc = subprocess.Popen(
            self.command,
            cwd=ROOT,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=None,
            bufsize=0,
        )
        self.pending.clear()

    def request(self, request: dict) -> dict:
        if self.proc is None or self.proc.poll() is not None:
            self.start()
        assert self.proc is not None and self.proc.stdin is not None and self.proc.stdout is not None
        payload = (json.dumps(request, separators=(",", ":"), sort_keys=True) + "\n").encode()
        try:
            self.proc.stdin.write(payload)
            self.proc.stdin.flush()
            deadline = time.monotonic() + self.timeout
            fd = self.proc.stdout.fileno()
            while True:
                newline = self.pending.find(b"\n")
                if newline >= 0:
                    line = bytes(self.pending[:newline])
                    del self.pending[:newline + 1]
                    return json.loads(line)
                remaining = deadline - time.monotonic()
                if remaining <= 0:
                    raise TimeoutError(f"request exceeded {self.timeout:g}s")
                ready, _, _ = select.select([fd], [], [], remaining)
                if not ready:
                    raise TimeoutError(f"request exceeded {self.timeout:g}s")
                chunk = os.read(fd, 65536)
                if not chunk:
                    raise RuntimeError("comparison worker exited without a response")
                self.pending.extend(chunk)
        except Exception:
            self.kill()
            raise

    def kill(self) -> None:
        if self.proc is not None:
            self.proc.kill()
            self.proc.wait()
            self.proc = None
        self.pending.clear()

    def close(self) -> None:
        if self.proc is not None:
            if self.proc.poll() is None:
                if self.proc.stdin:
                    self.proc.stdin.close()
                try:
                    self.proc.wait(timeout=2)
                except subprocess.TimeoutExpired:
                    self.proc.kill()
                    self.proc.wait()
            self.proc = None


def request_for(fixture: dict, method: str) -> dict:
    return {key: fixture[key] for key in ("case_id", "kind", "points", "terms") if key in fixture} | {"method": method}


def write_jsonl(path: Path, rows: list[dict]) -> None:
    with path.open("w", encoding="utf-8") as stream:
        for row in rows:
            stream.write(json.dumps(row, sort_keys=True, separators=(",", ":")) + "\n")


class JsonlLog:
    """Append each raw record immediately so an interrupted run stays useful."""

    def __init__(self, path: Path):
        self.stream = path.open("w", encoding="utf-8")

    def append(self, row: dict) -> None:
        self.stream.write(json.dumps(row, sort_keys=True, separators=(",", ":")) + "\n")
        self.stream.flush()

    def close(self) -> None:
        self.stream.close()


def command_metadata(command: list[str], cpu: int, run_dir: Path, seed: int,
                     repetitions: int, warmups: int, timeout: float) -> dict:
    def text_command(args: list[str]) -> str:
        try:
            return subprocess.run(args, cwd=ROOT, capture_output=True, text=True, timeout=5, check=False).stdout.strip()
        except (OSError, subprocess.TimeoutExpired):
            return "unavailable"

    cpu_model = "unavailable"
    try:
        for line in Path("/proc/cpuinfo").read_text().splitlines():
            if line.lower().startswith("model name"):
                cpu_model = line.split(":", 1)[1].strip()
                break
    except OSError:
        pass
    diff = text_command(["git", "diff", "--binary", "HEAD"])
    listed_sources = text_command(["git", "ls-files", "-m", "-o", "--exclude-standard"])
    source_paths = [line for line in listed_sources.splitlines() if line]
    source_digest = hashlib.sha256()
    for source in sorted(source_paths):
        source_digest.update(source.encode())
        source_digest.update(b"\0")
        try:
            source_digest.update((ROOT / source).read_bytes())
        except OSError:
            source_digest.update(b"<unreadable>")
    return {
        "started_utc": datetime.now(timezone.utc).isoformat(),
        "git_head": text_command(["git", "rev-parse", "HEAD"]),
        "git_base": text_command(["git", "merge-base", "HEAD", "origin/feature/tropical-viewer"]),
        "git_status_short": text_command(["git", "status", "--short"]),
        "dirty_diff_sha256": hashlib.sha256(diff.encode()).hexdigest(),
        "modified_or_untracked_files": sorted(source_paths),
        "modified_or_untracked_sha256": source_digest.hexdigest(),
        "command": command,
        "haskell_build_flags": "-O2; worker RTS: +RTS -N1 -RTS",
        "python": sys.version,
        "platform": platform.platform(),
        "cpu_model": cpu_model,
        "cpu_pinned": cpu,
        "ghc": text_command(["stack", "exec", "--", "ghc", "--numeric-version"]),
        "stack": text_command(["stack", "--version"]),
        "lscpu": text_command(["lscpu"]),
        "run_directory": str(run_dir),
        "seed": seed,
        "repetitions": repetitions,
        "warmups": warmups,
        "per_request_timeout_seconds": timeout,
    }


def timed_worker_command(binary: list[str], cpu: int) -> list[str]:
    command = binary + ["+RTS", "-N1", "-RTS"]
    if shutil.which("taskset"):
        return ["taskset", "-c", str(cpu)] + command
    return command


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", default="stack exec -- solver-comparison",
                        help="worker command (default: stack exec -- solver-comparison)")
    parser.add_argument("--seed", type=int, default=20261006)
    parser.add_argument("--repetitions", type=int, default=5)
    parser.add_argument("--warmups", type=int, default=1)
    parser.add_argument("--timeout", type=float, default=10.0, help="per request, seconds")
    parser.add_argument("--cpu", type=int, help="pin worker to this allowed CPU")
    parser.add_argument("--run-dir", type=Path, help="raw output directory (default under validation/)")
    parser.add_argument("--verifier", type=Path, default=COMPARISON / "verify.py")
    parser.add_argument("--correctness-only", action="store_true", help="stop after verified outputs; do not time methods")
    args = parser.parse_args()

    if args.repetitions < 1 or args.warmups < 0 or args.timeout <= 0:
        parser.error("repetitions must be positive; warmups nonnegative; timeout positive")
    allowed = sorted(os.sched_getaffinity(0)) if hasattr(os, "sched_getaffinity") else [0]
    cpu = args.cpu if args.cpu is not None else allowed[0]
    if cpu not in allowed:
        parser.error(f"CPU {cpu} is not in the process affinity set {allowed}")

    timestamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    run_dir = args.run_dir or (ROOT / "validation" / "solver-comparison" / f"{timestamp}-seed{args.seed}")
    run_dir.mkdir(parents=True, exist_ok=False)
    lock_path = ROOT / "validation" / "solver-comparison" / ".benchmark.lock"
    lock_path.parent.mkdir(parents=True, exist_ok=True)
    lock_stream = lock_path.open("w")
    fcntl.flock(lock_stream, fcntl.LOCK_EX)

    binary = shlex.split(args.binary)
    command = timed_worker_command(binary, cpu)
    fixtures = make_fixtures(args.seed)
    write_jsonl(run_dir / "fixtures.jsonl", fixtures)
    (run_dir / "metadata.json").write_text(json.dumps(command_metadata(
        command, cpu, run_dir, args.seed, args.repetitions, args.warmups, args.timeout), indent=2) + "\n")
    (run_dir / "run-status.json").write_text(json.dumps({"status": "running", "phase": "correctness"}, indent=2) + "\n")

    correctness: list[dict] = []
    correctness_log = JsonlLog(run_dir / "correctness.jsonl")
    worker = Worker(command, args.timeout)
    try:
        for fixture in fixtures:
            for method in fixture["methods"]:
                req = request_for(fixture, method)
                try:
                    row = worker.request(req)
                except TimeoutError as error:
                    worker.start()
                    row = {"case_id": fixture["case_id"], "method": method, "status": "timeout", "error": str(error)}
                except Exception as error:
                    worker.start()
                    row = {"case_id": fixture["case_id"], "method": method, "status": "error", "error": str(error)}
                correctness.append(row)
                correctness_log.append(row)
    finally:
        worker.close()
        correctness_log.close()

    report_path = run_dir / "verification.json"
    verifier = [sys.executable, str(args.verifier), "--fixtures", str(run_dir / "fixtures.jsonl"),
                "--results", str(run_dir / "correctness.jsonl"), "--output", str(report_path)]
    verified = subprocess.run(verifier, cwd=ROOT, text=True, capture_output=True, check=False)
    (run_dir / "verification.stdout.txt").write_text(verified.stdout)
    (run_dir / "verification.stderr.txt").write_text(verified.stderr)
    if verified.returncode != 0:
        (run_dir / "run-status.json").write_text(json.dumps({"status": "failed", "phase": "correctness",
                                                               "reason": "independent verifier failed"}, indent=2) + "\n")
        print(f"Correctness verification failed; raw results preserved in {run_dir}", file=sys.stderr)
        return 1

    if args.correctness_only:
        (run_dir / "run-status.json").write_text(json.dumps({"status": "correctness-only", "phase": "complete"}, indent=2) + "\n")
        print(f"Correctness checks passed; raw results saved in {run_dir}")
        return 0

    correctness_by_key = {(row.get("case_id"), row.get("method")): row for row in correctness}
    eligible = [(fixture, method) for fixture in fixtures for method in fixture["methods"]
                if method != "legacy" and correctness_by_key.get((fixture["case_id"], method), {}).get("status") == "ok"]
    rng = random.Random(args.seed ^ 0x5A17)
    measured_rows: list[dict] = []
    warmup_rows: list[dict] = []
    warmup_log = JsonlLog(run_dir / "warmups.jsonl")
    sample_log = JsonlLog(run_dir / "samples.jsonl")
    worker = Worker(command, args.timeout)
    failure: str | None = None
    try:
        warmup_failed = False
        for warmup_round in range(args.warmups):
            jobs = eligible[:]
            rng.shuffle(jobs)
            for fixture, method in jobs:
                try:
                    row = worker.request(request_for(fixture, method))
                except Exception as error:
                    worker.start()
                    row = {"case_id": fixture["case_id"], "method": method,
                           "status": "timeout" if isinstance(error, TimeoutError) else "error", "error": str(error)}
                warmup_rows.append({"phase": "warmup", "round": warmup_round, **row})
                warmup_log.append(warmup_rows[-1])
                reference = correctness_by_key[(fixture["case_id"], method)]
                if row.get("status") != "ok" or row.get("result") != reference.get("result"):
                    warmup_failed = True
                    failure = f"warmup failed or returned inconsistent output for {fixture['case_id']}:{method}"
                    break
            if warmup_failed:
                break
        for sample in range(0 if warmup_failed else args.repetitions):
            jobs = eligible[:]
            rng.shuffle(jobs)
            for fixture, method in jobs:
                try:
                    row = worker.request(request_for(fixture, method))
                except Exception as error:
                    worker.start()
                    row = {"case_id": fixture["case_id"], "method": method,
                           "status": "timeout" if isinstance(error, TimeoutError) else "error", "error": str(error)}
                record = {"phase": "measured", "sample": sample, **row}
                if row.get("status") != "ok":
                    measured_rows.append(record)
                    sample_log.append(record)
                    failure = f"measured request failed for {fixture['case_id']}:{method}"
                    break
                reference = correctness_by_key[(fixture["case_id"], method)]
                if row.get("result") != reference.get("result"):
                    record["status"] = "error"
                    record["consistency_error"] = "Repeated canonical output differs from correctness pass"
                    measured_rows.append(record)
                    sample_log.append(record)
                    failure = f"repeated output changed for {fixture['case_id']}:{method}"
                    break
                measured_rows.append(record)
                sample_log.append(record)
            else:
                continue
            break
    finally:
        worker.close()
        warmup_log.close()
        sample_log.close()
    grouped: dict[tuple[str, str], list[int]] = {}
    for row in measured_rows:
        if row.get("status") == "ok":
            grouped.setdefault((row["case_id"], row["method"]), []).append(int(row["wall_ns"]))
    summary = []
    from statistics import median
    for (case_id, method), samples in sorted(grouped.items()):
        summary.append({"case_id": case_id, "method": method, "n": len(samples),
                        "median_wall_ns": int(median(samples)),
                        "median_cpu_ps": int(median(int(row["cpu_ps"]) for row in measured_rows
                                                      if row.get("case_id") == case_id and row.get("method") == method
                                                      and row.get("status") == "ok"))})
    completed_samples = all(len(grouped.get((fixture["case_id"], method), [])) == args.repetitions
                            for fixture, method in eligible)
    if not completed_samples and failure is None:
        failure = "one or more case/method pairs have an incomplete repetition count"
    summary_obj = {
        "status": "failed" if failure else "complete",
        "failure": failure,
        "seed": args.seed,
        "repetitions": args.repetitions,
        "warmups": args.warmups,
        "timeout_seconds": args.timeout,
        "fixture_count": len(fixtures),
        "correctness_results": len(correctness),
        "unsupported_correctness_results": sum(row.get("status") == "unsupported" for row in correctness),
        "timed_method_cases": len(grouped),
        "sample_rows": len(measured_rows),
        "methods": summary,
    }
    (run_dir / "summary.json").write_text(json.dumps(summary_obj, indent=2) + "\n")
    (run_dir / "run-status.json").write_text(json.dumps({"status": summary_obj["status"], "phase": "timing",
                                                           "reason": failure}, indent=2) + "\n")
    print(f"Saved reproducible run to {run_dir}")
    return 1 if failure else 0


if __name__ == "__main__":
    raise SystemExit(main())
