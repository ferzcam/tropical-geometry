# Tropical

Haskell library for tropical algebra and computational geometry.

## Dependencies

The project pins Stack resolver `lts-23.19` in `stack.yaml`. Its existing system
dependency instructions include the OpenGL libraries:

```sh
sudo apt-get install libglu1-mesa-dev freeglut3-dev mesa-common-dev
```

## Tests

With the project dependencies installed:

```sh
stack test --no-terminal --no-install-ghc --only-locals --no-prefetch \
  --jobs 2 --test-arguments='--timeout=30s -j2 +RTS -N2 -RTS'
```

The verified suite has **133 enabled, passing cases**, including independent
polytope fixtures, restored geometry cases and explicit invalid-input tests.
See [PROGRESS.md](PROGRESS.md) for validation logs, coverage, known limits and
checkpoint/rollback instructions. This verification used an existing environment,
not a fresh dependency installation.

## LRS input contract

`Geometry.LRS.lrs` uses exact rational inequalities **Ax <= b** and requires a
feasible starting vertex with enough independent tight constraints. It selects
that independent basis even when more than d facets meet at the vertex.

For example, the unit square is:

```haskell
lrs (fromLists [[-1,0],[0,-1],[1,0],[0,1]])
    (colFromList [0,0,1,1]) [0,0]
-- [[0,0],[0,1],[1,0],[1,1]]
```

Use `fromLists` from `Data.Matrix` and `lrs`/`colFromList` from
`Geometry.LRS`.

Supported outputs are bounded-polytope vertices or homogeneous pointed-cone ray
directions. For cones, every bound in b must be zero; ray lengths may vary by
positive scale. General unbounded inputs need a separate vertex/ray result type
and are rejected. Lower-dimensional facet enumeration uses intrinsic coordinates
and adds paired inequalities for affine-hull equalities. These equality pairs
contain all input vertex IDs, rather than identifying intrinsic facets.

Subdivision and hypersurface construction preserve convex polygonal cells,
including squares and hexagons. The plotting API still stores `Int` coordinates:
nonintegral fan vertices are explicitly rejected instead of rounded. Affine
reconstruction and LRS use rational arithmetic; intrinsic extreme-point filtering
still uses the existing GLPK backend.
