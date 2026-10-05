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

The verified suite has **97 enabled, passing cases**, including independent
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
and are rejected. The facet enumeration APIs reject lower-dimensional input
pending affine reduction.

The older subdivision/hypersurface path currently assumes triangular cells;
general polygonal subdivisions are not covered by this contract.
