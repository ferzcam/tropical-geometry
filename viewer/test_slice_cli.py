"""Exact section oracle: compare original 3D ties with returned curves/regions."""
from fractions import Fraction as Q
import json
import random
import subprocess
import threading
import unittest
import urllib.error
import urllib.request
from pathlib import Path

from test_geometry_cli import BACKEND, on_edge
from serve import ViewerServer


def original_tie(terms, x, y, height):
    normalized = {}
    for t in terms:
        key = t['x'], t['y'], t['z']
        c = Q(t['coefficient'])
        normalized[key] = min(c, normalized.get(key, c))
    values = [c+a*x+b*y+k*height for (a,b,k),c in normalized.items()]
    return values.count(min(values)) >= 2


def in_region(p, region):
    return all(Q(h['a'])*p[0]+Q(h['b'])*p[1] <= Q(h['bound'])
               for h in region['inequalities'])


@unittest.skipUnless(BACKEND, 'Set TROPICAL_BACKEND for exact 3D slice checks')
class SliceOracle(unittest.TestCase):
    def compute(self, terms, height):
        run = subprocess.run([BACKEND], input=json.dumps({'terms':terms,'height':str(height)}),
                             text=True, capture_output=True, timeout=15, check=True)
        data = json.loads(run.stdout)
        self.assertNotIn('error', data, data)
        self.assertEqual(Q(data['height']), height)
        return data

    def test_original_three_dimensional_ties_match_section(self):
        cases = [
            [(0,0,0,0),(1,0,0,0),(0,1,0,0),(0,0,1,0)],
            [(0,0,0,0),(0,0,1,0)],
            [(0,0,0,0),(0,0,0,0)],
            [(0,0,0,1),(0,0,1,1),(0,0,2,0)],
            [(0,0,0,0),(0,0,1,0),(1,0,0,0),(-1,0,0,0)],
            [(0,0,0,0),(0,0,1,0),(1,0,0,0),(-1,0,0,0),
             (0,1,0,0),(0,-1,0,0)],
        ]
        rng = random.Random(314159)
        support = [(x,y,z) for x in range(-1,2) for y in range(-1,2) for z in range(-1,2)]
        for _ in range(12):
            cases.append([(x,y,z,Q(rng.randrange(-2,3),rng.randrange(1,4)))
                          for x,y,z in rng.sample(support, rng.randrange(2,7))])
        for case in cases:
            terms = [dict(x=x,y=y,z=z,coefficient=str(c)) for x,y,z,c in case]
            for height in [Q(-1),Q(0),Q(1,3),Q(1)]:
                with self.subTest(terms=terms,height=height):
                    data = self.compute(terms,height)
                    for ix in range(-4,5):
                        for iy in range(-4,5):
                            p = Q(ix,2),Q(iy,2)
                            represented = any(on_edge(p,e) for e in data['edges']) or any(
                                in_region(p,r) for r in data['regions'])
                            self.assertEqual(original_tie(terms,*p,height), represented, (terms,height,p,data))
                    for region in data['regions']:
                        source = [data['sourceTerms'][i] for i in region['terms']]
                        self.assertGreaterEqual(len(source),2)
                        self.assertEqual(len({(t['x'],t['y']) for t in source}),1)
                        self.assertEqual(len({Q(t['coefficient'])+t['z']*height for t in source}),1)

    def test_exceptional_plane_and_generic_empty_slices(self):
        terms = [dict(x=0,y=0,z=k,coefficient='0') for k in [0,1]]
        at_zero = self.compute(terms,Q(0))
        self.assertEqual(at_zero['edges'],[])
        self.assertEqual(len(at_zero['regions']),1)
        self.assertTrue(in_region((Q(12345),Q(-54321)),at_zero['regions'][0]))
        for height in [Q(-1),Q(1)]:
            data = self.compute(terms,height)
            self.assertEqual(data['regions'],[])
            self.assertEqual(data['edges'],[])

    def test_explicit_invalid_height_cannot_be_parsed_as_a_curve(self):
        terms = [dict(x=0,y=0,z=k,coefficient='0') for k in [0,1]]
        for height in [None,0,False,'1/0']:
            with self.subTest(height=height):
                run = subprocess.run([BACKEND],input=json.dumps(dict(terms=terms,height=height)),
                                     text=True,capture_output=True,timeout=15,check=True)
                self.assertIn('error',json.loads(run.stdout))

    def test_http_slice_route_and_validation(self):
        server = ViewerServer(('127.0.0.1',0),Path(__file__).parent,BACKEND)
        thread = threading.Thread(target=server.serve_forever,daemon=True)
        thread.start()
        base = f'http://127.0.0.1:{server.server_address[1]}'
        terms = [dict(x=0,y=0,z=k,coefficient='0') for k in [0,1]]
        def post(payload):
            request = urllib.request.Request(base+'/api/slice',data=json.dumps(payload).encode(),
                                             headers={'Content-Type':'application/json'})
            return urllib.request.urlopen(request,timeout=20)
        try:
            with post(dict(terms=terms,height='0')) as response:
                self.assertEqual(len(json.load(response)['regions']),1)
            for payload in [dict(terms=terms),dict(terms=terms,height='1/0'),
                            dict(terms=terms,height=0),dict(terms=terms,height='0',extra=True),
                            dict(terms=[dict(x=0,y=0,z=True,coefficient='0')],height='0')]:
                with self.subTest(payload=payload):
                    with self.assertRaises(urllib.error.HTTPError) as error:
                        post(payload)
                    self.assertEqual(error.exception.code,400)
        finally:
            server.shutdown()
            server.server_close()
            thread.join(timeout=2)


if __name__ == '__main__':
    unittest.main()
