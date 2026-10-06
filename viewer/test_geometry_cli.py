"""Independent exact oracle checks. Set TROPICAL_BACKEND to the built CLI path.

Example: TROPICAL_BACKEND="$(stack path --local-install-root)/bin/tropical-viewer-geometry" \
python3 -m unittest discover -s viewer -p 'test_*.py'
"""
from fractions import Fraction as Q
import json
import os
import random
import subprocess
import unittest

BACKEND = os.environ.get("TROPICAL_BACKEND")


def point(value):
    return tuple(map(Q, value))


def on_edge(p, edge):
    start = point(edge["start"])
    direction = (tuple(b - a for a, b in zip(start, point(edge["end"])))
                 if edge["kind"] == "segment" else point(edge["direction"]))
    delta = tuple(b - a for a, b in zip(start, p))
    if delta[0] * direction[1] != delta[1] * direction[0]:
        return False
    axis = 0 if direction[0] else 1
    if not direction[axis]:
        return p == start
    t = delta[axis] / direction[axis]
    return edge["kind"] == "line" or (t >= 0 and (edge["kind"] == "ray" or t <= 1))


def tied(terms, p):
    # Distinct exponents; duplicate monomials are normalized by their minimum coefficient.
    normalized = {}
    for term in terms:
        key = term["x"], term["y"]
        normalized[key] = min(normalized.get(key, Q(term["coefficient"])), Q(term["coefficient"]))
    values = [c + x * p[0] + y * p[1] for (x, y), c in normalized.items()]
    return values.count(min(values)) >= 2


def signature(curve):
    vertices = sorted(point(v["point"]) for v in curve["vertices"])
    edges = sorted((e["kind"], point(e["start"]), point(e.get("end", e.get("direction"))), int(e["weight"])) for e in curve["edges"])
    return vertices, edges


@unittest.skipUnless(BACKEND, "Set TROPICAL_BACKEND to test the real Haskell CLI")
class GeometryCLIOracle(unittest.TestCase):
    def compute(self, terms):
        result = subprocess.run([BACKEND], input=json.dumps({"terms": terms}), capture_output=True,
                                text=True, timeout=15, check=True)
        curve = json.loads(result.stdout)
        self.assertNotIn("error", curve, curve)
        return curve

    def check_geometry(self, terms):
        curve = self.compute(terms)
        edges = curve["edges"]
        for x in range(-2, 3):
            for y in range(-2, 3):
                p = Q(x, 2), Q(y, 2)
                self.assertEqual(tied(terms, p), any(on_edge(p, edge) for edge in edges), (terms, p, curve))
        for edge in edges:
            start = point(edge["start"])
            if edge["kind"] == "segment":
                end = point(edge["end"])
                samples = [start, end, tuple((a+b)/2 for a,b in zip(start,end))]
            else:
                d = point(edge["direction"])
                samples = [tuple(a+t*b for a,b in zip(start,d)) for t in ([0,Q(1,3),3] if edge["kind"] == "ray" else [-3,0,Q(1,3),3])]
            for p in samples:
                self.assertTrue(tied(terms, p), (terms, edge, p))
            a, b = [curve["terms"][i] for i in edge["dual"]]
            import math
            self.assertEqual(int(edge["weight"]), math.gcd(abs(a["x"]-b["x"]), abs(a["y"]-b["y"])))
        for vertex in curve["vertices"]:
            p = point(vertex["point"])
            balance = [Q(0), Q(0)]
            for edge in edges:
                if edge["kind"] == "line":
                    continue
                start = point(edge["start"])
                if edge["kind"] == "ray":
                    if start != p:
                        continue
                    d = point(edge["direction"])
                else:
                    end = point(edge["end"])
                    if p not in (start,end):
                        continue
                    other = end if p == start else start
                    d = tuple(b-a for a,b in zip(p,other))
                    import math
                    scale = math.lcm(d[0].denominator,d[1].denominator)
                    integers = [int(v*scale) for v in d]
                    gcd = math.gcd(*map(abs,integers))
                    d = tuple(Q(v,gcd) for v in integers)
                for axis in range(2):
                    balance[axis] += int(edge["weight"])*d[axis]
            self.assertEqual(balance, [0,0], (terms,p,curve))
        return curve

    def test_seeded_polynomials_against_fraction_oracle(self):
        rng = random.Random(20261006)
        cases = [[(0,0,0)], [(0,0,0),(2,0,1)], [(0,0,0),(1,0,0),(2,0,0)],
                 [(0,0,0),(2,0,-1),(0,2,-1)], [(0,0,0),(1,0,0),(1,1,0),(0,1,0)]]
        support = [(x,y) for x in range(-2,3) for y in range(-2,3)]
        for _ in range(20):
            cases.append([(x,y,Q(rng.randrange(-4,5),rng.randrange(1,4))) for x,y in rng.sample(support,rng.randrange(2,8))])
        for case in cases:
            terms = [{"x":x,"y":y,"coefficient":str(c)} for x,y,c in case]
            with self.subTest(terms=terms):
                original = self.check_geometry(terms)
                permuted = list(reversed(terms))
                self.assertEqual(signature(original),signature(self.compute(permuted)))
                shifted = [dict(t,coefficient=str(Q(t["coefficient"])+Q(7,3))) for t in terms]
                self.assertEqual(signature(original),signature(self.compute(shifted)))

    def test_cli_invalid_inputs_and_fraction_strings(self):
        for terms in [[], [{"x":False,"y":0,"coefficient":"0"}],
                      [{"x":0,"y":0,"coefficient":"1/0"}],
                      [{"x":0,"y":0,"coefficient":"١"}],
                      [{"x":0,"y":0,"coefficient":"1/02"}]]:
            with self.subTest(terms=terms):
                result = subprocess.run([BACKEND],input=json.dumps({"terms":terms}),capture_output=True,text=True,timeout=15,check=True)
                self.assertIn("error",json.loads(result.stdout))
        curve = self.compute([{"x":0,"y":0,"coefficient":"0"},{"x":2,"y":0,"coefficient":"-1"},{"x":0,"y":2,"coefficient":"-1"}])
        self.assertEqual(curve["vertices"][0]["point"],["1/2","1/2"])


if __name__ == "__main__":
    unittest.main()
