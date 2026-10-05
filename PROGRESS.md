# LRS correctness progress

## Current status — 2026-10-05

**All 97 enabled tests pass.** There are no expected-failure wrappers or skipped
registered cases. The baseline had 36 enabled tests.

The source is on `fix/lrs-correctness`, reviewed in [draft PR #3](https://github.com/ferzcam/tropical-geometry/pull/3).
The fixed baseline branch `baseline/lrs-2026-10-05` preserves checkpoint
`c55eed45f3d3ca431b67a0d0b689669dab691a1e`, including the previously unfinished
P3/P4 fixtures. The original local branch contained 13 commits beyond the remote
`upgrade_lts`; the separate PR base preserves that history without updating an
existing remote branch. Integrating the PR into `upgrade_lts` is a separate step.

Final evidence: [full test log](validation/final-supported-suite/stack-test.log),
[command, timing and source hashes](validation/final-supported-suite/metadata.json),
[enabled test inventory](validation/final-supported-suite/test-inventory.json).

Validation used Stack 3.1.1, GHC 9.8.4 and lts-23.19 with installed dependencies.
Changed Haskell modules were rebuilt, but this is not a clean-environment or
cross-platform reproducibility claim.

```sh
stack test --no-terminal --no-install-ghc --only-locals --no-prefetch \
  --jobs 2 --test-arguments='--timeout=30s -j2 +RTS -N2 -RTS'
```

## Completed changes

1. Enabled P3/P4 unchanged and recorded their failures before fixing them.
2. Standardized LRS inputs on Ax <= b, matching the dictionary and facet generator.
   The hand-written >= fixtures were converted by negating both A and b.
3. Corrected ray extraction to use current cobasic variable IDs and negate the
   decision-column entries, as specified in Avis Proposition 3.2.
4. Selected d independent tight rows for the initial basis. The original valid
   final-d-row basis is retained where possible; redundant rows retain their
   order and multiplicity. The existing lexicographic block was sufficient.
5. Added explicit errors for malformed RHS/start data and rank-deficient starts.
   Both facet APIs reject unsupported lower-dimensional inputs by exact affine rank.
6. Fixed the four/five-point hull empty-list crash and merged whole coplanar facet
   groups into correctly oriented convex boundaries, removing facet-interior points.
7. Rejected unbounded non-homogeneous inputs whose vertices would otherwise be
   silently lost in the flat vertex/ray output API.

## Recorded progression

| Change | Commit | Pass / total | Evidence |
| --- | --- | --- | --- |
| Preserved baseline | c55eed4 | 36 / 36 | [log](validation/baseline-2026-10-05/stack-test.log) |
| Enabled P3/P4 unchanged | 8889243 | 36 / 38 | [failures](validation/permutohedra-red/stack-test.log) |
| Convention and ray correction | 08ef545 | 38 / 38 | [log](validation/convention-fix/stack-test.log) |
| Expanded and restored tests | 9eab547 | 75 / 88 | [failures](validation/expanded-red/stack-test.log) |
| Independent tight basis | c241530 | 81 / 88 | [log](validation/independent-basis/stack-test.log) |
| Explicit facet dimension guard | 258dca2 | 85 / 88 | [log](validation/facet-dimension-guard/stack-test.log) |
| Small-hull initialization | 8a12d8f | 87 / 88 | [log](validation/small-hulls/stack-test.log) |
| Complete coplanar merging | 2d7df36 | 88 / 88 | [log](validation/coplanar-hulls/stack-test.log) |
| Cyclic polytopes and cone invariance | 0bd16f3 | 95 / 95 | [log](validation/final-coverage/stack-test.log) |
| Exposed mixed-output limitation | f52352b | 95 / 97 | [failures](validation/unbounded-red/stack-test.log) |
| Explicit mixed-output rejection | See PR head | 97 / 97 | [log](validation/final-supported-suite/stack-test.log) |

## Coverage and historical disabled cases

| Area | Current treatment |
| --- | --- |
| P3/P4 | Active; exact 6/24 vertices, all starts, row permutations and positive rational row scaling |
| Cross-polytopes in 4D/5D | Active; exact 8/10 vertices, all starts and reversed rows |
| Hypersimplex Delta(2,4), Birkhoff B3 | Active; independent exact expected vertices, all starts and reversed rows |
| Cyclic C(6,3), C(8,4) | Active; independently generated exact facets, all starts and reversed rows |
| Simplex products, translated/sheared shapes | Active; independent Cartesian/permutation fixtures |
| Cone rays | Active; nonzero directions, feasibility, row order/scaling invariance |
| Newton f1/f2/f3/f6/f7/f8 | Original tests retained and passing |
| Newton f4/f9-style lifted supports | Active rejection tests through both facet APIs; affine reduction is not implemented |
| Historical Laurent f5 | Its valid subdivision and tropical rays are active; old duplicate Newton-port notes remain historical |
| Three disabled 2D hull assertions | Restored as separate active cases |
| Valid 3D tetrahedron/cube assertions | Restored; additional five-point hull regression |
| Old list1 projected expectation | Replaced by an independently verified 11-vertex full-hull test; old example retained as a comment |
| Four projected-coordinate 3D expectations | Historical examples only: they change z coordinates and are not full-hull specifications |
| Cube adjacent facets | Active unordered facet-membership test |
| f4/f5 subdivisions and hypersurfaces | Active; ray directions tested independently of display segment length |
| Malformed/infeasible/interior starts | Active explicit rejection tests |
| Unbounded strip and shifted quadrant | Active explicit rejection tests for unsupported mixed output |

The old “axis-aligned only” and “missing perturbation block” diagnoses are
superseded. Non-axis-aligned and degenerate families now pass; no new perturbation
block was required.

Independent fixture checks are reproducible with:

```sh
python3 test-suite/fixtures/list1_oracle.py
python3 test-suite/fixtures/tropical-cyclic-oracle.py > /tmp/cyclic-regenerated.hs
cmp /tmp/cyclic-regenerated.hs test-suite/TGeometry/TLRSCyclic.hs
```

The Newton tests still share `extremalVertices` with their upstream construction;
the independent polytope fixtures complement, rather than replace, those tests.

## Boundaries and follow-up work

- The checked `lrs` API supports bounded full-dimensional polytopes and pointed
  cones represented with every RHS bound equal to zero. Other unbounded inputs
  fail explicitly. A future API should return vertices and rays separately.
  Even a redundant nonzero bound on an otherwise equivalent cone falls outside
  the current cone representation contract.
- Lower-dimensional facet enumeration requires affine reduction/equalities.
  It is explicitly rejected rather than represented by incorrect offset planes.
- The legacy subdivision path still assumes triangular cells:
  `projectionToR2` filters nontriangular faces and hypersurface helpers destructure
  triples. The restored triangle cases pass; general polygonal subdivisions are
  separate work. Do not interpret the passing suite as support for them.
- Invalid projected 3D expectations were not blindly activated. The legacy
  coplanar fallback is specialized for the polynomial projection path, not a
  general affine-hull implementation.
- A clean-environment build, external polymake end-to-end checks and wider
  performance validation remain future work.

## Rollback

The workstation backup is `~/tropical-geometry-checkpoints/2026-10-05-baseline/`.
It contains the original HEAD/status, binary patch, full repository bundle,
tracked-source archive and verified SHA-256 manifest.

Keep the baseline branch fixed. Each fix is a separate commit with its validation
log. Inspect and preserve any uncommitted work first, then use
`git revert <fix-commit>` to undo a change and rerun the suite. Do not hard-reset,
clean the checkout, rewrite history or force-push.

For a separate baseline inspection:

```sh
git worktree add --detach ../tropical-geometry-baseline c55eed45f3d3ca431b67a0d0b689669dab691a1e
```
