# Solver comparison protocol

See [measured results and historical discrepancies](RESULTS.md) for the findings.

`Main.hs` is a JSON-lines worker for exact comparisons. `run.py` creates a
seeded fixture set, saves one correctness result per requested method, checks
those outputs with `verify.py`, and only then records timings. Raw run files
are written beneath `validation/solver-comparison/`, which is ignored by Git.

The first fixture set compares the tailored and LRS hull routes in two and
three dimensions, and the exact, tailored, and LRS tropical-curve routes in
two variables. It also compares the exact, reconstructed original general
hypersurface route, and LRS graph one-skeletons in three variables. These are
1D graph skeletons, not full 2D hypersurfaces and not horizontal slices. The
legacy bivariate
`Polynomial.Hypersurface` output is checked on integral line, square, cubic,
and original benchmark f1 fixtures; it is excluded from performance summaries.
Rank-deficient and fractional fixtures remain in the correctness record,
where expected domain exclusions are reported as unsupported. An unexpected
`Left` result or exception is an error and prevents timing.

The reconstructed three-variable route uses the existing Yang/GLPK hull
pipeline with corrected graph assembly. It is not the unchanged historical
wrapper: that wrapper inserts ray directions into its vertex list and can miss
adjacency when opposite facet normals have different scales. The source-faithful
negative cases and corrected results are retained in `HistoricalRegression.hs`,
from historical commit `25892444db7813bb1819df344d36e1fe93400198`.

Run from the repository root after the `solver-comparison` executable and
`comparison/verify.py` are available:

```sh
stack build tropical-geometry:exe:solver-comparison --ghc-options=-O2
stack exec -- solver-comparison --self-test
python3 comparison/verify.py --self-test
python3 comparison/run.py
```

The global `--ghc-options=-O2` override also optimizes the library, so all
methods use the same optimization setting. The LRS adapter uses bounded polar
duality with an independently found feasible starting vertex; see the
[LRS user guide](https://cgm.cs.mcgill.ca/~avis/C/lrslib/USERGUIDE.html).
This measures this repository's Haskell LRS implementation, not C lrslib.

The default command uses `stack exec -- solver-comparison`; override it with
`--binary`, for example `--binary 'stack exec -- solver-comparison'`. Use
`--seed`, `--repetitions`, `--warmups`, `--timeout`, and `--cpu` to change the
recorded protocol parameters. The default run has one warmup and five measured
rounds, a 10-second per-request limit, and pins the worker to the first CPU in
the caller's allowed CPU set. A lock serializes concurrent invocations.
Use `--correctness-only` to save and independently verify all outputs without
starting the timing phase.

Each fixture is sent as a fresh JSON value to one persistent worker. The
worker parses and forces inputs before timing, runs one method, forces every
output field before stopping the monotonic wall and CPU clocks, then serializes
the exact canonical output after timing. The executable must be built with
`-O2`; the runner starts it with `+RTS -N1 -RTS`. These measurements include
all work performed by the selected adapter, including LRS preparation and
enumeration, while excluding process startup, JSON parsing, and JSON output.
This is a serialized single-core comparison, not a throughput benchmark.

The run folder contains:

* `fixtures.jsonl`: exact inputs, stable case IDs, and the methods checked;
* `correctness.jsonl`: the complete canonical correctness outputs;
* `verification.json` and verifier stdout/stderr: independent oracle checks;
* `warmups.jsonl`: outputs from unmeasured warmup requests;
* `samples.jsonl`: every measured output, including its complete geometry,
  wall nanoseconds, CPU picoseconds, and sample number;
* `metadata.json`: source revision and dirty-diff digest, machine/CPU and GHC
  information, worker command, seed, and CPU pin;
* `summary.json`: per-case medians and run counts computed from raw samples.

JSONL records are flushed after every request. A failed verifier, warmup,
repeat-consistency check, timeout, or incomplete sample count writes a failed
run status and makes the driver exit nonzero; the raw records remain available.

The fixture generator uses only Python's standard library. Its seeded hull
clouds exercise 8, 16, 24, and 32 input points. Fixed fixtures include a
triangle, square, tetrahedron, cube, duplicate points, fractional coordinates,
collinear points, and coplanar 3D points. Tropical fixtures include a standard
line, flat and lifted squares, a cubic lattice support, original benchmark
f1, rational coefficients, and seeded supports of 8, 16, and 32 terms.
Three-variable fixtures include simplex and fractional-root examples, a
quadratic lattice support, duplicates, rank-deficient support, and seeded
supports of 8 and 16 terms. The exact verifier uses independent rational facet
enumeration and root-locus or graph-skeleton checks; it is a correctness oracle
for these modest fixtures, not a timed method. Run the Haskell checks with
`stack exec -- solver-comparison --self-test`.

Do not compare timings from different compiler flags, CPU models, or fixture
seeds as if they were a controlled solver comparison. Keep the run directory
with any reported timing so every input, output, repeat, and metadata record
remains available for reanalysis.
