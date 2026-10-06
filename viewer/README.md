# Tropical curve explorer

Explore a finite min-plus polynomial in two variables. Each term is
coefficient + a*x + b*y. The curve consists of points where at least two
distinct exponent terms attain the minimum.

## Run

From the repository root:

```sh
stack build
python3 viewer/serve.py
```

Open http://127.0.0.1:8765. Python 3.9 or later is required. The server listens
on loopback only and uses the compiled Haskell executable from Stack's local
install directory. An explicit path can be supplied with
`--backend /path/to/tropical-viewer-geometry`; use `--port` to change the port.

When running on a remote machine, forward the same port with
`ssh -L 8765:127.0.0.1:8765 user@host`, then open the local address.
The viewer's runtime assets are bundled; npm is only needed for browser tests.

## Explore

Select an example or edit the exponent/coefficient table and draw the polynomial.
Coefficients accept integers and exact fractions such as `-3/2`.
Repeated exponent vectors are combined by retaining the smallest coefficient.
Labels in the plots refer to the normalized terms displayed beneath the views.

The left board shows segments, rays, and lines of the root locus. The right board
shows the dual subdivision, including polygonal cells without artificial
triangulation. Select a curve vertex to highlight its dual cell, or an edge to
highlight its dual edge. Edge weights are the integer lattice lengths of dual edges.

Use the navigation controls or mouse wheel to zoom; Shift-drag pans.
Each board has a reset button and an SVG export button. Exports reflect the
current viewport and include the exact geometry as metadata. Display positions
use browser floating-point arithmetic; labels and exported metadata retain fractions.

Single monomials have an empty root locus. Collinear exponent supports can
produce complete parallel lines, rather than vertices with outgoing rays.

## Exact geometry and API

`Geometry.TropicalCurve.tropicalCurve` accepts `Term` values with integer
exponents and rational coefficients. `Polynomial.Curve.tropicalCurveOf`
adapts existing two-variable tropical polynomial values to this representation.

The algorithm intersects each pair of affine terms' equality line with the
inequalities requiring those terms to attain the global minimum. Exact rational
interval clipping gives segments, rays, or full lines. Dual cells are recovered
from terms attaining the minimum at each vertex. This path does not call LRS or
the legacy GLPK-based hull builder.

The pure API accepts up to 64 terms and arbitrary integer exponents. The local
HTTP/CLI interface limits requests to 32 terms, exponents from -100 to 100,
and coefficients with at most 18 digits in each numerator/denominator.
Denominators must be positive without leading zeros. These limits bound
interactive computation; they do not round input.

POST `/api/curve` with JSON, for example:

```json
{"terms":[{"x":0,"y":0,"coefficient":"0"},{"x":2,"y":0,"coefficient":"1"},{"x":0,"y":1,"coefficient":"0"}]}
```

The response contains normalized `terms`, `vertices`, `edges`, and `cells`.
Coordinates and edge weights are strings, preserving exact fractions/integers.
Edges include their `kind` (segment, ray, or line), incident term indices, and
`dual` endpoint indices. Cells contain a CCW `boundary` of term indices and the
associated curve `vertex`. IDs are zero-based and deterministic under input
reordering, but may change when coefficients change the geometry.

## Tests

```sh
stack test --test-arguments='--timeout=30s -j2 +RTS -N2 -RTS'
TROPICAL_BACKEND="$(stack path --local-install-root)/bin/tropical-viewer-geometry" \
  python3 -m unittest discover -s viewer -p 'test_*.py'
cd viewer
npm ci
npx playwright install chromium
npm test
```

Browser tests require the viewer running at http://127.0.0.1:8765. Set
`VIEWER_URL` to test a different port. Without `TROPICAL_BACKEND`, the Python
suite skips its two real-backend integration tests.
Geometry tests check exact fractional vertices, polygonal duality, minimum
attainment, weighted balancing, degenerate supports, and normalization.

## Bundled dependency

`vendor/` contains JSXGraph 1.14.0 from its npm release, used under the MIT
license. The original license notices are included. `vendor/manifest.json`
records the archive integrity and SHA-256 hashes of the bundled files.
