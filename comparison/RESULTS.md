# Solver comparison results

The tailored hull algorithms, exact tropical solver, and Haskell LRS agree on
the supported fixtures described below. The historical three-variable wrapper
has two reproducible bugs; its corrected comparison adapter agrees with the
other methods. Performance depends on the task: tailored hulls win the
representative hull cases, whereas LRS beats the reconstructed general
three-variable pipeline. The direct exact root solver is fastest in the root
examples measured here.

## What was compared

* **2D and 3D hulls:** existing `ConvexHull2`/`ConvexHull3` versus LRS polar
  duality, comparing exact canonical supporting facets.
* **Two-variable tropical curves:** direct pairwise equality clipping versus
  the tailored lifted 3D hull and LRS lifted 3D hull. Both hull routes use
  shared exact rational dualization. Full canonical vertices, weighted edges,
  rays, lines, and subdivision cells are compared. The actual current legacy
  `Polynomial.Hypersurface` output is also checked on five integral fixtures,
  but is not timed: its integer coordinates and finite ray drawings differ
  from the exact API's contract.
* **Three-variable tropical roots:** the complete graph **1-skeleton**,
  matching the historical API's contract, using independent triple clipping,
  the reconstructed Yang/GLPK pipeline, and LRS lifted 4D hulls. This is neither
  a full two-dimensional tropical surface nor a horizontal slice. The
  reconstructed route uses current fixed geometry modules and corrected graph
  assembly; it is not the unchanged historical wrapper.

The LRS route uses this repository's Haskell implementation, not C lrslib.
Its seed search and polar conversion do not call the tailored hull algorithms.

## Historical discrepancies

Source-faithful regressions from historical commit
`25892444db7813bb1819df344d36e1fe93400198` reproduce two failures:

1. The wrapper includes ray direction vectors in its vertex list. For
   `min(0, 2x-1, 3y-2, 4z-3)`, the one true vertex is `(1/2, 2/3, 3/4)`;
   the old wrapper reports five vertices, including four direction vectors.
2. Adjacency requires opposite normals of exactly equal magnitude. For
   `min(z, x+z, y+z, 1, 1+3z)`, adjacent cells produce normals `(0,0,4)` and
   `(0,0,-2)`. The correct graph has vertices `(0,0,-1/2)` and `(0,0,1)`,
   one bounded segment, and six rays. The old graph emits no bounded segment
   and eight rays.

[HistoricalRegression.hs](HistoricalRegression.hs) preserves both failures
and checks the corrected original, direct, and LRS outputs against explicit
expected geometry. The historical branch itself has not been modified.

## Correctness and validation

Each of two seeded runs contains 37 fixtures and 96 correctness requests:
87 supported outputs pass, with 48 cross-method comparisons and no errors or
mismatches. Across both runs this is 174 checked outputs and 96 comparisons.
Independent rational checks enumerate supporting hull facets, validate curve
geometry and weights, and exhaustively enumerate three-variable finite vertices
through four-term ties. Three-variable edges are checked by samples, a finite
grid, and complete cross-method equality; an independent exhaustive proof of
edge completeness is not claimed.

Nine requests per run are explicitly unsupported: rank-deficient hull inputs
(four requests), fractional coefficients in the tailored curve and original
general routes (two), and coplanar three-variable exponent support (three).
Three fixtures therefore have no supported method. Full-rank restrictions here
belong to the comparison adapters, not a blanket claim about existing hull APIs.

Validation also passed: **169 Haskell tests, 15 Python tests, 18 browser tests,
16 comparison self-checks**, and independent verifier corruption checks.
These finite fixtures establish regression evidence, not universal equivalence.

## Timings

Representative **median wall milliseconds over seven repetitions**, seed
20261006; two warmups per method-case pair:

| Fixture | Tailored / reconstructed original | Direct exact | LRS |
| --- | ---: | ---: | ---: |
| 2D hull, 32 points | 0.277 | — | 19.742 |
| 3D hull, 32 points | 38.018 | — | 76.090 |
| Two-variable cubic, 10 terms | 2.910 | 0.227 | 3.467 |
| Three-variable quadratic 1-skeleton, 10 terms | 26.740 | 0.869 | 8.356 |

Here the tailored hulls are about 71× and 2× faster than LRS. For the cubic,
the tailored route is about 1.19× faster than LRS. For the three-variable
quadratic, LRS is about 3.20× faster than the reconstructed original route.
The direct solver is faster than either hull route on these root examples.

Seed 20261007 gives the same ordering for these examples. Its hull point
clouds differ, so their times are not repeated measurements of identical
inputs. The fixed cubic and quadratic inputs are identical across seeds.
These small fixtures do not establish asymptotic scaling or a universal winner.

Both runs completed all 82 timed method-case pairs, seven measurements each:
574 samples per run, **1,148 samples total**. An independent raw-data analysis
checked sample counts, result consistency, and reported medians.

## Reproduction and evidence

Measured code checkpoint: **`222a546`**, clean working tree during the runs.
Machine: AMD Ryzen Threadripper PRO 5965WX, 24 physical cores / 48 logical CPUs.
GHC 9.8.4, Stack resolver lts-23.19; global `-O2` applies to the library and
executable. Requests run serially in a persistent worker pinned to CPU 0,
with `+RTS -N1 -RTS`. Inputs are parsed and forced before timing; full results
are forced inside timing, then serialized afterward. Timings include adapter
normalization, LRS seed preparation, enumeration, and canonicalization.

From the repository root on the workstation:

```sh
stack build tropical-geometry:exe:solver-comparison --ghc-options=-O2
stack exec -- solver-comparison --self-test
python3 comparison/verify.py --self-test
python3 comparison/run.py --binary "$(stack path --local-install-root)/bin/solver-comparison" \
  --seed 20261006 --repetitions 7 --warmups 2 \
  --run-dir validation/solver-comparison/verified-seed20261006
python3 comparison/run.py --binary "$(stack path --local-install-root)/bin/solver-comparison" \
  --seed 20261007 --repetitions 7 --warmups 2 \
  --run-dir validation/solver-comparison/verified-seed20261007
```

The two run directories above retain fixtures, full correctness outputs,
verification reports, warmups, every timed output, metadata, and summaries.
They are local ignored artifacts, not included in Git. Agent notes and raw
analysis are likewise ignored under `.agent-work/solver-comparison/`.
See the [comparison protocol](README.md) for output contracts, timing details,
and failure handling. No viewer backend has been switched by this comparison.
