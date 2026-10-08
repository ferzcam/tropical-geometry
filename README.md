# Tropical

Haskell library for tropical algebra and computational geometry.

## Interactive tropical curves and surfaces

The browser viewer displays a tropical curve beside its Newton subdivision,
with linked selection, exact fraction labels, editable coefficients, JSON file
import/export, pan/zoom, and SVG export. A genus-1 cubic example shows a bounded cycle. JSXGraph and three.js are bundled locally; the running viewer needs no internet.

Build the geometry executable, then launch the local viewer:

```sh
stack build
python3 viewer/serve.py
```

Open <http://127.0.0.1:8765>. The [3D graph explorer](http://127.0.0.1:8765/graph3.html)
renders the exact one-skeleton (vertices and edges, not the two-dimensional
sheets) of a three-variable tropical surface beside its dual Newton
subdivision in two linked three.js views. The [3D slice explorer](http://127.0.0.1:8765/slices.html)
shows horizontal sections of three-variable tropical hypersurfaces as the height changes.
On the 2D and 3D graph pages a **Method** control selects the direct solver,
the tailored convex-hull route, or the Haskell LRS route; each runs as named
and reports inputs outside its contract (the hull routes need integer
coefficients, and the 3D hull route refuses subdivisions its legacy facet
enumeration leaves incomplete) instead of substituting another method. See [the viewer guide](viewer/README.md)
for examples, the JSON API, method contracts, tests, and input limits.

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

The verified suite has **199 enabled, passing cases**, including independent
polytope fixtures, restored geometry cases, explicit invalid-input tests, and
cross-method checks of the three-variable graph one-skeleton with its dual cells.
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

`Geometry.TropicalGraph3` returns the exact one-skeleton of a three-variable
tropical hypersurface together with the dual three-dimensional cells and
two-dimensional faces of its Newton subdivision (`exactGraph3`, `lrsGraph3`),
and `Geometry.TropicalHull3.hullGraph3` feeds the legacy GLPK/Yang hull
pipeline into the same exact assembly. This is not the full two-dimensional
surface. The exponent support must have affine rank three and inputs are
limited to 32 terms. The legacy Yang facet enumeration is known to miss a
lower facet on the lifted genus-one cubic (see
[the comparison results](comparison/RESULTS.md)); the library route reports
that case as an error rather than returning partial geometry.

## Solver comparisons

See the [correctness and timing results](comparison/RESULTS.md) for comparisons
between the tailored hull algorithms, direct exact tropical solvers, and Haskell
LRS in two and three dimensions. The [reproduction protocol](comparison/README.md)
includes the supported domains and the three-variable graph-skeleton contract.
