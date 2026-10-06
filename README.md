# Tropical

Haskell library for tropical algebra and computational geometry.

## Interactive tropical curves

The browser viewer displays a tropical curve beside its Newton subdivision,
with linked selection, exact fraction labels, editable coefficients, JSON file
import/export, pan/zoom, and SVG export. A genus-1 cubic example shows a bounded cycle. JSXGraph is bundled locally; the running viewer needs no internet.

Build the geometry executable, then launch the local viewer:

```sh
stack build
python3 viewer/serve.py
```

Open <http://127.0.0.1:8765>. The [3D slice explorer](http://127.0.0.1:8765/slices.html)
shows horizontal sections of three-variable tropical hypersurfaces as the height changes. See [the viewer guide](viewer/README.md)
for examples, the JSON API, tests, and input limits.

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

The verified suite has **169 enabled, passing cases**, including independent
polytope fixtures, restored geometry cases and explicit invalid-input tests.
This verification used an existing environment, not a fresh dependency installation.
The suite includes affine hulls, degenerate polytopes, coplanar hulls, and polygonal
subdivisions; current API limitations are described below.

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
including squares and hexagons. The legacy Gloss plotting API still stores `Int` coordinates and rejects
nonintegral fan vertices. The new `Geometry.TropicalCurve` API and browser viewer
retain exact rational coordinates, including fractional vertices. Affine
reconstruction and LRS use rational arithmetic; intrinsic extreme-point filtering
still uses the existing GLPK backend.

## Solver comparisons

See the [correctness and timing results](comparison/RESULTS.md) for comparisons
between the tailored hull algorithms, direct exact tropical solvers, and Haskell
LRS in two and three dimensions. The [reproduction protocol](comparison/README.md)
includes the supported domains and the three-variable graph-skeleton contract.
