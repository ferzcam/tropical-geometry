# Tropical curve explorer

Explore a finite min-plus polynomial in two variables. Each term is
coefficient + a*x + b*y. The curve consists of points where at least two
distinct exponent terms attain the minimum. Two companion pages extend this to
three variables: the [3D graph explorer](#three-variable-graph-explorer) at
`/graph3.html` and the [horizontal slices](#horizontal-slices-of-three-variable-polynomials)
at `/slices.html`. The header of each page links to the other two.

## Pages and computation methods

| Page | Dimension | Output | Methods |
| --- | --- | --- | --- |
| `/` | 2 variables | exact curve and dual Newton subdivision | direct, tailored hull, LRS |
| `/graph3.html` | 3 variables | exact graph one-skeleton and dual cells/faces | direct, tailored hull, LRS |
| `/slices.html` | 3 variables | exact horizontal section at a height | direct only |

The **Method** control names the library route that runs:

* **direct** – `Geometry.TropicalCurve.tropicalCurve` (2D) and
  `Geometry.TropicalGraph3.exactGraph3` (3D): exact pairwise/triple equality
  clipping; no hull algorithm.
* **hull** – the tailored convex-hull pipelines:
  `Geometry.TropicalHull2.hullTropicalCurve` (incremental `convexHull3` on the
  lifted integer points, lower faces projected with `projectionToR2`) and
  `Geometry.TropicalHull3.hullGraph3` (the reconstructed generalized route:
  GLPK extreme points plus Yang facet enumeration on the lifted 4D points and on
  each projected cell).
* **lrs** – `Geometry.TropicalCurve.lrsTropicalCurve` and
  `Geometry.TropicalGraph3.lrsGraph3`: this repository's Haskell LRS through
  polar duality on the lifted points and on each projected cell.

Every response echoes the `method` that produced it, and the status line names
it. A method never falls back to another one: inputs outside its contract
return an error. The hull routes require integer coefficients within the
machine `Int` range (fractions are rejected), and the 3D hull route reports
subdivisions its legacy facet enumeration does not close (see below). The hull
routes only supply lower subdivision cells; dual vertices, rays, weights,
incidences, and IDs are recomputed and validated exactly by the shared
assembly, so all methods return identical records where they succeed.

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

## Horizontal slices of three-variable polynomials

Open **Explore 3D slices** or visit `/slices.html`. Choose one of the
demonstrations and move the height slider or enter an exact rational height.
The graph stays in the x-y plane while z is fixed at the selected height.
The cubic example has a loop at z=0, which collapses at z=1.

The page shows the actual intersection with the original tropical hypersurface.
At special heights, distinct three-variable monomials may become identical
affine functions of x and y. Their joint minimum can occupy a filled region,
not just a curve. Such regions are shaded and clipped to the current viewport.
For example, `min(0,x,y,z)` at z=0 includes the nonnegative quadrant;
`min(0,z)` at z=0 includes the entire plane and at other heights is empty.

The curve skeleton and its subdivision are computed after specialization.
The additional shaded regions retain the original three-dimensional term
identities. This is a horizontal-section viewer, not a full 3D surface renderer.

POST `/api/slice` with `terms` containing integer `x`, `y`, and `z`
exponents, rational-string `coefficient`, and a rational-string `height`.
The existing limits apply to each exponent, coefficient, and height.
The response includes the specialized curve, canonical `height`,
normalized `sourceTerms`, and candidate `regions` as exact inequalities
`a*x + b*y <= bound`. A candidate can be empty or lower-dimensional;
the page fills only its visible positive-area intersection with the viewport.

## Three-variable graph explorer

Open **Explore 3D graphs** or visit `/graph3.html`. The page shows two linked
three.js views: the tropical graph on the left and the dual Newton subdivision
on the right. Drag to orbit, right-drag (or two-finger drag) to pan, and scroll
or pinch to zoom; each view has a reset button and a PNG export.

**What is shown is the one-skeleton only.** The backend computes the vertices
and edges of the tropical surface: vertices are dual to the three-dimensional
cells of the regular Newton subdivision, bounded edges to faces shared by two
cells, and rays to faces on the boundary of the Newton polytope. The
two-dimensional sheets of the surface, dual to subdivision edges, are not
computed or drawn. The page states this contract above the views, and the API
marks every response with `"contract": "one-skeleton"`.

Spheres are graph vertices, tubes are bounded edges, and tubes ending in a cone
are rays (drawn to a display length; the exact direction is listed below the
views). The right view draws every two-dimensional face as its true convex
polygon, with no triangulation edges, plus numbered term points. Hover over or
click any object in either view, or use the buttons below, to highlight the
dual pair: a vertex with its cell (all faces of the cell take the vertex
color), or an edge with its face (shared color). Coordinates in the labels are
exact fractions; only the drawing is rounded.

The examples are the tropical plane `min(0,x,y,z)`, a fractional vertex, a
split quadratic with one bounded edge, the lifted genus-one cubic that the
slices page cuts (four cells, three bounded edges), and the integer paraboloid
lift of the 3×3×3 grid (eight cube cells). **Import polynomial** and **Export
JSON** use `{"terms":[{"x":..,"y":..,"z":..,"coefficient":".."}]}`, the same
term format as `/api/slice`; two example files are linked from the page.

Input limits for all three methods: 1 to 32 terms, exponents within ±100, and
exponent support of affine rank three (a polynomial whose exponents lie in a
plane is rejected; use the 2D page for such inputs). The lifted support must
have affine rank three (one flat cell) or four. Complete lines cannot occur
with rank-three support and are reported as an error if a route produces one.

Known limitation of the **hull** method in three variables: the legacy
GLPK/Yang facet enumeration can miss a lower facet of the lifted 4D support.
On the lifted genus-one cubic it returns three of the four cells. The exact
assembly detects that a face has only one enumerated cell although exponents
lie beyond it and returns an error (`missed a lower cell`) instead of drawing
a spurious ray; choose the direct or LRS method for that input. The 2D hull
method has no known such case.

POST `/api/graph3` with `terms` (integer `x`, `y`, `z` and rational-string
`coefficient`) and an optional `method` of `direct`, `hull`, or `lrs`:

```json
{"terms":[{"x":0,"y":0,"z":0,"coefficient":"0"},{"x":1,"y":0,"z":0,"coefficient":"-1"},{"x":2,"y":0,"z":0,"coefficient":"0"},{"x":0,"y":1,"z":0,"coefficient":"0"},{"x":0,"y":0,"z":1,"coefficient":"0"}],"method":"lrs"}
```

The response contains `kind`, `contract`, `method`, normalized `terms` (with
`id`), `vertices` (`id`, exact `point`, minimizing `terms`, dual `cell`),
`edges` (`id`, `kind` segment or ray, exact `start` and `end` or primitive
integer `direction`, `terms`, dual `face`, incident `vertices`), `cells`
(`id`, dual `vertex`, `terms`, `faces`), and `faces` (`id`, `terms`, cyclic
convex `boundary` of term IDs, incident `cells`, dual `edge`). Cell IDs equal
their vertex IDs and face IDs equal their edge IDs. Lists are sorted
deterministically, so IDs are stable under input reordering. The CLI accepts
the same object with `"kind":"graph3"` added; `/api/curve` likewise accepts an
optional `method` and echoes it.

## Import and save polynomials

Use **Import polynomial** to open a JSON file; a valid file fills the editor and
draws the curve automatically. **Export JSON** downloads the current terms,
preserving coefficient strings such as `1/3`. Files use the same `terms`
format as the API example below. Malformed or unsupported files leave the
existing polynomial intact.

Try [the genus-1 cubic](examples/genus-1-cubic.json), also available from the
viewer as a download and a built-in example. Its coefficients are a² + a*b + b²
for all nonnegative exponent pairs with a+b at most 3. Its tropical curve has one
bounded hexagonal cycle near (-3,-3), visibly exhibiting genus 1.

Files must be JSON objects containing only `terms`, up to 32 KiB in size,
and meet the same term/exponent/coefficient limits as the local API.

## Exact geometry and API

`Geometry.TropicalCurve.tropicalCurve` accepts `Term` values with integer
exponents and rational coefficients. `Polynomial.Curve.tropicalCurveOf`
adapts existing two-variable tropical polynomial values to this representation.

The direct algorithm intersects each pair of affine terms' equality line with the
inequalities requiring those terms to attain the global minimum. Exact rational
interval clipping gives segments, rays, or full lines. Dual cells are recovered
from terms attaining the minimum at each vertex. This path does not call LRS or
the legacy GLPK-based hull builder. `lrsTropicalCurve` and
`Geometry.TropicalHull2.hullTropicalCurve` compute the same record from LRS
and tailored-hull lower cells; the hull route additionally requires integer
coefficients and at least three non-collinear exponents.

The pure API accepts up to 64 terms and arbitrary integer exponents. The local
HTTP/CLI interface limits requests to 32 terms, exponents from -100 to 100,
and coefficients with at most 18 digits in each numerator/denominator.
Denominators must be positive without leading zeros. These limits bound
interactive computation; they do not round input.

POST `/api/curve` with JSON, for example:

```json
{"terms":[{"x":0,"y":0,"coefficient":"0"},{"x":2,"y":0,"coefficient":"1"},{"x":0,"y":1,"coefficient":"0"}]}
```

The response contains the `method` that ran, normalized `terms`, `vertices`, `edges`, and `cells`.
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
suite skips its real-backend integration tests. The Playwright configuration
launches Chromium with software WebGL (`--enable-unsafe-swiftshader`) because
headless Chromium without a GPU drops WebGL contexts; the 3D page reports a
lost context in its status line instead of showing blank panels.
Geometry tests check exact fractional vertices, polygonal duality, minimum
attainment, weighted balancing, degenerate supports, and normalization. The
3D tests compare all three methods' full records, recheck every reported
incidence against the exact minimum, exercise method routing and error
reporting through the CLI and HTTP server, and drive both three.js views
(selection, picking, method switching, imports, exports, navigation, and a
narrow viewport) in headless Chromium.

## Bundled dependencies

`vendor/` contains JSXGraph 1.14.0 from its npm release, used under the MIT
license. The original license notices are included. `vendor/manifest.json`
records the archive integrity and SHA-256 hashes of the bundled files.
`vendor/three/` contains `three.module.js`, `three.core.js`, and
`OrbitControls.js` from the three.js 0.186.1 npm release (MIT), loaded through
an import map; `vendor/three/manifest.json` records their hashes. The viewer
never loads anything from the network.
