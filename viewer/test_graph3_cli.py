"""Exact 3D graph oracle and routing checks against the real backend and server."""
from fractions import Fraction as Q
import json
from pathlib import Path
import subprocess
import threading
import unittest
import urllib.error
import urllib.request

from serve import ViewerServer, backend_request, validate_request
from test_geometry_cli import BACKEND

ROOT = Path(__file__).resolve().parent


def term(x, y, z, c):
    return dict(x=x, y=y, z=z, coefficient=str(c))


SIMPLEX = [term(0, 0, 0, 0), term(1, 0, 0, 0), term(0, 1, 0, 0), term(0, 0, 1, 0)]
SHIFTED = [term(0, 0, 0, 0), term(1, 0, 0, '-1/2'), term(0, 1, 0, '-2/3'), term(0, 0, 1, '-3/4')]
SPLIT = json.loads((ROOT / 'examples/split-quadratic-3d.json').read_text())['terms']
CUBIC = json.loads((ROOT / 'examples/lifted-cubic-3d.json').read_text())['terms']
UNEQUAL = [term(0, 0, 1, 0), term(1, 0, 1, 0), term(0, 1, 1, 0), term(0, 0, 0, 1), term(0, 0, 3, 1)]


def active(terms, p):
    normalized = {}
    for t in terms:
        key = t['x'], t['y'], t['z']
        normalized[key] = min(normalized.get(key, Q(t['coefficient'])), Q(t['coefficient']))
    ordered = sorted(normalized)
    values = [normalized[k] + k[0] * p[0] + k[1] * p[1] + k[2] * p[2] for k in ordered]
    least = min(values)
    return [i for i, v in enumerate(values) if v == least]


@unittest.skipUnless(BACKEND, 'Set TROPICAL_BACKEND for exact 3D graph checks')
class Graph3Oracle(unittest.TestCase):
    def run_backend(self, payload):
        run = subprocess.run([BACKEND], input=json.dumps(payload), text=True, capture_output=True, timeout=30, check=True)
        return json.loads(run.stdout)

    def compute(self, terms, method=None):
        payload = backend_request(validate_request(dict(terms=terms, **({'method': method} if method else {})), 'graph3'), 'graph3')
        data = self.run_backend(payload)
        self.assertNotIn('error', data, data)
        self.assertEqual(data['kind'], 'graph3')
        self.assertEqual(data['contract'], 'one-skeleton')
        self.assertEqual(data['method'], method or 'direct')
        return data

    def check_incidences(self, terms, data):
        vertices, edges, cells, faces = data['vertices'], data['edges'], data['cells'], data['faces']
        self.assertEqual([v['id'] for v in vertices], list(range(len(vertices))))
        self.assertEqual([c['vertex'] for c in cells], [v['id'] for v in vertices])
        self.assertEqual([f['edge'] for f in faces], [e['id'] for e in edges])
        for vertex, cell in zip(vertices, cells):
            p = tuple(map(Q, vertex['point']))
            self.assertEqual(active(terms, p), vertex['terms'], (terms, vertex))
            self.assertEqual(cell['terms'], vertex['terms'])
            self.assertEqual(cell['faces'], [f['id'] for f in faces if cell['id'] in f['cells']])
            self.assertGreaterEqual(len(cell['faces']), 4)
        for edge, face in zip(edges, faces):
            self.assertIn(edge['kind'], ('segment', 'ray'))
            start = tuple(map(Q, edge['start']))
            if edge['kind'] == 'segment':
                end = tuple(map(Q, edge['end']))
                samples = [tuple((a + b) / 2 for a, b in zip(start, end))]
                endpoints = [start, end]
            else:
                direction = tuple(map(Q, edge['direction']))
                samples = [tuple(a + t * d for a, d in zip(start, direction)) for t in (Q(1, 3), 4)]
                endpoints = [start]
            for p in samples:
                self.assertEqual(active(terms, p), edge['terms'], (terms, edge, p))
            self.assertEqual(edge['terms'], face['terms'])
            self.assertEqual(edge['vertices'], face['cells'])
            self.assertEqual(sorted(v['id'] for v in vertices if tuple(map(Q, v['point'])) in endpoints), edge['vertices'])
            self.assertEqual(len(face['cells']), 2 if edge['kind'] == 'segment' else 1)
            self.assertGreaterEqual(len(face['boundary']), 3)
            self.assertTrue(set(face['boundary']) <= set(face['terms']))
            for c in face['cells']:
                self.assertTrue(set(face['terms']) <= set(cells[c]['terms']))
            direction = (tuple(b - a for a, b in zip(start, end)) if edge['kind'] == 'segment'
                         else tuple(map(Q, edge['direction'])))
            exponents = [(data['terms'][i]['x'], data['terms'][i]['y'], data['terms'][i]['z']) for i in face['terms']]
            for q in exponents:
                self.assertEqual(sum((a - b) * d for a, b, d in zip(q, exponents[0], direction)), 0)

    def test_routes_agree_and_satisfy_incidence_invariants(self):
        for terms in [SIMPLEX, SPLIT, CUBIC, UNEQUAL]:
            with self.subTest(terms=terms):
                direct = self.compute(terms)
                self.check_incidences(terms, direct)
                # The legacy GLPK/Yang facet enumeration misses one lower cell of
                # the lifted cubic; the backend must report that, not draw it.
                for method in ('lrs',) if terms is CUBIC else ('lrs', 'hull'):
                    other = self.compute(terms, method)
                    for key in ('terms', 'vertices', 'edges', 'cells', 'faces'):
                        self.assertEqual(direct[key], other[key], (method, key))
        self.assertEqual(len(self.compute(CUBIC)['cells']), 4)
        cubic_hull = self.run_backend(backend_request(dict(terms=CUBIC, method='hull'), 'graph3'))
        self.assertIn('missed a lower cell', cubic_hull['error'])
        shifted = self.compute(SHIFTED)
        self.check_incidences(SHIFTED, shifted)
        self.assertEqual(shifted['vertices'][0]['point'], ['1/2', '2/3', '3/4'])
        for key in ('vertices', 'edges', 'cells', 'faces'):
            self.assertEqual(shifted[key], self.compute(SHIFTED, 'lrs')[key])

    def test_tropical_plane_and_split_quadratic_counts(self):
        plane = self.compute(SIMPLEX)
        self.assertEqual((len(plane['vertices']), len(plane['edges']), len(plane['cells']), len(plane['faces'])), (1, 4, 1, 4))
        self.assertEqual([len(f['boundary']) for f in plane['faces']], [3, 3, 3, 3])
        split = self.compute(SPLIT)
        self.assertEqual((len(split['vertices']), len(split['edges']), len(split['cells']), len(split['faces'])), (2, 7, 2, 7))
        self.assertEqual([e['kind'] for e in split['edges']].count('segment'), 1)
        self.assertEqual(len([f for f in split['faces'] if len(f['cells']) == 2]), 1)

    def test_unsupported_inputs_name_the_route_and_do_not_fall_back(self):
        hull = self.run_backend(backend_request(dict(terms=SHIFTED, method='hull'), 'graph3'))
        self.assertIn('integral', hull['error'])
        self.assertIn('hullGraph3', hull['error'])
        for method in ('direct', 'lrs', 'hull'):
            with self.subTest(method=method):
                data = self.run_backend(backend_request(dict(terms=SIMPLEX[:3], method=method), 'graph3'))
                self.assertIn('affine rank three', data['error'])
        self.assertIn('Unknown method', self.run_backend(dict(kind='graph3', terms=SIMPLEX, method='other'))['error'])
        self.assertIn('Unknown request kind', self.run_backend(dict(kind='other', terms=SIMPLEX))['error'])
        curve = self.run_backend(dict(terms=[dict(x=0, y=0, coefficient='0'), dict(x=1, y=0, coefficient='1/3'), dict(x=0, y=1, coefficient='0')], method='hull'))
        self.assertIn('integral', curve['error'])

    def test_curve_methods_report_the_route_that_ran(self):
        terms = [dict(x=0, y=0, coefficient='0'), dict(x=1, y=0, coefficient='0'), dict(x=0, y=1, coefficient='0'), dict(x=1, y=1, coefficient='0'), dict(x=2, y=0, coefficient='1')]
        results = {method: self.run_backend(dict(terms=terms, method=method)) for method in ('direct', 'hull', 'lrs')}
        for method, data in results.items():
            self.assertNotIn('error', data, data)
            self.assertEqual(data['method'], method)
            for key in ('vertices', 'edges', 'cells', 'terms'):
                self.assertEqual(data[key], results['direct'][key], (method, key))
        self.assertEqual(self.run_backend(dict(terms=terms))['method'], 'direct')

    def test_http_graph3_route_assets_and_validation(self):
        server = ViewerServer(('127.0.0.1', 0), ROOT, BACKEND)
        thread = threading.Thread(target=server.serve_forever, daemon=True)
        thread.start()
        base = f'http://127.0.0.1:{server.server_address[1]}'

        def post(path, payload):
            request = urllib.request.Request(base + path, data=json.dumps(payload).encode(), headers={'Content-Type': 'application/json'})
            return urllib.request.urlopen(request, timeout=30)
        try:
            with post('/api/graph3', dict(terms=SPLIT, method='lrs')) as response:
                data = json.load(response)
                self.assertEqual(data['method'], 'lrs')
                self.assertEqual(len(data['cells']), 2)
            with post('/api/curve', dict(terms=[dict(x=0, y=0, coefficient='0'), dict(x=2, y=0, coefficient='1'), dict(x=0, y=1, coefficient='0')], method='lrs')) as response:
                self.assertEqual(json.load(response)['method'], 'lrs')
            with post('/api/slice', dict(terms=SIMPLEX, height='0')) as response:
                self.assertEqual(len(json.load(response)['edges']), 3)
            with self.assertRaises(urllib.error.HTTPError) as error:
                post('/api/graph3', dict(terms=SHIFTED, method='hull'))
            self.assertEqual(error.exception.code, 422)
            self.assertIn('integral', json.load(error.exception)['error'])
            for path, payload in [('/api/graph3', dict(terms=SPLIT, method='other')), ('/api/graph3', dict(terms=SPLIT, kind='graph3')),
                                  ('/api/graph3', dict(terms=[dict(x=0, y=0, coefficient='0')])), ('/api/curve', dict(terms=SPLIT)),
                                  ('/api/curve', dict(terms=[dict(x=0, y=0, coefficient='0')], method=1)), ('/api/slice', dict(terms=SIMPLEX, height='0', method='lrs'))]:
                with self.subTest(path=path, payload=payload):
                    with self.assertRaises(urllib.error.HTTPError) as error:
                        post(path, payload)
                    self.assertEqual(error.exception.code, 400)
            for path in ('/graph3.html', '/graph3.js', '/graph3.css', '/slices.html', '/index.html',
                         '/vendor/three/three.module.js', '/vendor/three/three.core.js', '/vendor/three/OrbitControls.js',
                         '/examples/split-quadratic-3d.json', '/examples/lifted-cubic-3d.json'):
                with self.subTest(path=path):
                    with urllib.request.urlopen(base + path, timeout=30) as response:
                        self.assertEqual(response.status, 200)
                        body = response.read()
                        self.assertEqual(body, (ROOT / path[1:]).read_bytes())
                        if path.endswith('.js'):
                            self.assertEqual(response.headers['Content-Type'], 'text/javascript')
        finally:
            server.shutdown()
            server.server_close()
            thread.join(timeout=2)


class VendorManifest(unittest.TestCase):
    def test_bundled_three_js_matches_manifest_hashes(self):
        import hashlib
        manifest = json.loads((ROOT / 'vendor/three/manifest.json').read_text())
        for name, digest in manifest['sha256'].items():
            with self.subTest(file=name):
                self.assertEqual(hashlib.sha256((ROOT / 'vendor/three' / name).read_bytes()).hexdigest(), digest)
        source = (ROOT / 'vendor/three/three.module.js').read_text()
        self.assertIn("from './three.core.js'", source)
        self.assertIn("from 'three'", (ROOT / 'vendor/three/OrbitControls.js').read_text())


if __name__ == '__main__':
    unittest.main()
