# LRS correctness progress

## Current status

Independent starting-basis selection: **81 pass, 7 fail out of 88**.
All cross-polytope, hypersimplex and Birkhoff cases now pass, including
multiple starting vertices and reversed constraint rows. The existing
lexicographic dictionary was sufficient; no new perturbation block was added.
Remaining failures concern hulls (three) and missing facet rejection (four).
Evidence: [basis log](validation/independent-basis/stack-test.log).


Expanded regression checkpoint: **75 pass, 13 fail out of 88 enabled tests**.
This includes 21 independent bounded-shape cases, eight degenerate-family cases,
nine input-validation cases and 12 restored/corrected legacy cases.
All are ordinary tests; known failures are not marked as expected successes.

Failures: five singular-basis cases, two small-hull crashes, one hull retaining
two facet-interior points, four missing lower-dimensional facet rejections, and
one insufficiently clear interior-start error. The last already rejects the
input but lacks the planned diagnostic. Evidence: [expanded log](validation/expanded-red/stack-test.log).


P3/P4 were enabled without changing their inputs or LRS in commit `8889243`.
The full suite then produced **36 passes and 2 failures** (38 cases): both
permutohedra return only their starting vertex. This is the recorded failing
regression checkpoint, not a new algorithm change.

Evidence: [activation log](validation/permutohedra-red/stack-test.log) and
[run metadata](validation/permutohedra-red/metadata.json).

The convention correction now passes all **38 enabled tests**. All handwritten
inputs use Ax <= b; malformed/infeasible starts are rejected. Ray extraction
uses the current cobasis and the negative decision-column entries, following
Avis Proposition 3.2. Pivot and lexicographic ratio logic are unchanged.

Evidence: [fix log](validation/convention-fix/stack-test.log) and
[metadata](validation/convention-fix/metadata.json). Wider regression coverage
and degenerate-start basis selection are next.

## Verified baseline — 2026-10-05

- Source checkpoint: `c55eed45f3d3ca431b67a0d0b689669dab691a1e`.
- Baseline branch: `baseline/lrs-2026-10-05` (keep fixed).
- Working branch: `fix/lrs-correctness`.
- Result: **36 enabled tests pass**, including **11 LRS tests**; exit status 0.
- Environment: Stack 3.1.1, GHC 9.8.4, resolver lts-23.19.
- Validation used the existing up-to-date build, not a clean environment rebuild.
- This historical baseline predates the fixes recorded in Current status.

The checkpoint preserves the previously uncommitted P3/P4 fixture definitions exactly.
The prior local branch contains 13 commits beyond GitHub's `upgrade_lts`. The draft
PR targets the separate baseline branch so those historical changes are not confused
with new fixes. Merging that PR will not update `upgrade_lts`; integration into the
release branch is a separate final step.

Evidence: [test output](validation/baseline-2026-10-05/stack-test.log),
[test inventory](validation/baseline-2026-10-05/test-inventory.json),
[run metadata](validation/baseline-2026-10-05/stack-test-metadata.json),
[environment](validation/baseline-2026-10-05/environment.json).

Reproduce using the existing installed dependencies:

```sh
stack test --no-terminal --no-install-ghc --only-locals --no-prefetch \
  --jobs 2 --test-arguments='+RTS -N2 -RTS'
```

## Progress and acceptance criteria

| Stage | Status | Acceptance |
| --- | --- | --- |
| Preserve checkout and verify baseline | Complete | Backup, checkpoint, saved full log and inventory |
| Make inequality convention consistent | Complete (38 tests pass) | Document Ax <= b; convert handwritten >= fixtures; validate starting feasibility; P3/P4 return exact 6/24 vertices |
| Strengthen bounded-polytope regressions | Pending | Multiple starting vertices, row orderings, positive row scaling, transformed polytopes and simplex products; check output feasibility |
| Audit ray enumeration | Pending | Check cobasic-variable selection, direction sign, feasibility and exact expected rays under the same convention |
| Audit degenerate vertices | Pending | Independent tight-basis selection and lexicographic behavior; cross-polytopes and additional valid families |
| Establish lower-dimensional behavior | Pending | Explicit, tested rejection under the current full-dimensional scope, or a separately justified extension |
| Triage older disabled assertions | Pending | Restore geometrically valid cases; correct invalid expectations from independent geometry |
| Independent final validation | Pending | All agreed supported cases enabled and passing; unsupported cases tested explicitly; independent fixture/oracle checks |

The first repair hypothesis is supported by source algebra, not runtime validation:
`getDictionary` builds `A*x + slack = b`, matching nonnegative slack for Ax <= b.
`Facet.checkBranch` explicitly checks <= 1 on centered points. Handwritten LRS
fixtures instead use >= constraints. Negating both their A and b is the candidate
conversion; changing the global slack sign previously regressed existing tests.

Do not treat the older “axis-aligned only” explanation as a proven algorithmic
restriction. Likewise, the extent of missing degeneracy support requires a fresh
audit: the dictionary already carries an identity-derived block through pivots.

## Enabled and disabled coverage

The enabled LRS cases are the cone, square, cube, tesseract, simplex, and Newton
polynomials f1, f2, f3, f6, f7 and f8. The other 25 enabled cases cover arithmetic,
polynomials, hulls, subdivision and hypersurface helpers.

These omissions are separate from the 36 passing cases:

| Area | Baseline state | Required follow-up |
| --- | --- | --- |
| Permutohedra P3/P4 | Complete definitions commented out in `TLRSPol2.hs` | Enable after convention correction; expect 6/24 vertices |
| Cross-polytopes in 4D/5D | Definitions commented out | Investigate degeneracy; expect 8/10 vertices |
| Newton f4/f9 | Defined but omitted; coplanar lifted points | Test explicit unsupported-input behavior under current scope |
| Newton f5 | Present only in stale historical comments | Establish intended case and expected geometry before activation |
| 2D hull | Three commented assertions | Check intended contract before restoration |
| 3D hull | Six old commented assertions plus `list1` assertion | Several expectations project/change coordinates and are invalid full-hull expectations; do not enable unchanged |
| Adjacent facets | Commented case | Determine appropriate order-insensitive comparison |
| Subdivision f4/f5 | Commented assertions | Verify expectations independently |
| Full hypersurface f4/f5 | Commented case | Verify expectations independently |
| Simplex products, hypersimplex, cyclic and Birkhoff families | Planned, not implemented | Add explicit independent fixtures; separate valid supported cases from unsupported inputs |

The old `list1` hull expectation includes (0,0,1), although all input points have
x+y >= 2. It cannot be a full-hull vertex. This assertion was already disabled
before this baseline; its removal is not a new fix.

The Newton tests use `extremalVertices` both upstream and to derive expected
vertices. Their passing results demonstrate pipeline consistency, not a fully
independent oracle. Preserve them and add independent expected sets.

## Checkpoint and rollback

The external backup on the workstation is
`~/tropical-geometry-checkpoints/2026-10-05-baseline/`. It contains the original
HEAD/status, binary working-tree patch, repository bundle, tracked working-tree
archive and SHA-256 manifest. The original working tree had no untracked files.

- Keep the baseline branch fixed and do not rewrite or force-push history.
- Use one focused commit per fix with its regression tests and logged result.
- Inspect `git status` before any rollback and preserve later uncommitted work.
- To undo an isolated committed fix, use `git revert <fix-commit>` and rerun the suite.
- To inspect the original code without disturbing the current checkout, create a
  separate worktree at the checkpoint:
  `git worktree add --detach ../tropical-geometry-baseline c55eed45f3d3ca431b67a0d0b689669dab691a1e`.
- Do not use a hard reset or clean operation to roll back.
- Update this file with every stage's commit, exact test result and remaining failures.
