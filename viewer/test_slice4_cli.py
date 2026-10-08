from fractions import Fraction as Q
import json, subprocess, unittest
from test_geometry_cli import BACKEND

def is_tie(ts,p,h):
    v=[Q(t["coefficient"])+t["x"]*p[0]+t["y"]*p[1]+t["z"]*p[2]+t["w"]*h for t in ts]
    return v.count(min(v))>1

def in_patch(p,q):
    e=q["equality"]
    return (sum(Q(e[k])*p[i] for i,k in enumerate(("a","b","c")))==Q(e["bound"]) and
            all(sum(Q(a[k])*p[i] for i,k in enumerate(("a","b","c")))<=Q(a["bound"]) for a in q["inequalities"]))

@unittest.skipUnless(BACKEND,"Set TROPICAL_BACKEND for the 4D slice backend")
class Slice4Oracle(unittest.TestCase):
    def test_returned_patches_match_exact_sample_ties(self):
        ts=[dict(x=x,y=y,z=z,w=w,coefficient="0") for x,y,z,w in [(0,0,0,0),(1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1)]]
        for h in (Q(-1),Q(0),Q(1,2),Q(1)):
            raw=subprocess.run([BACKEND],input=json.dumps(dict(terms=ts,height=str(h))),text=True,capture_output=True,check=True,timeout=15)
            data=json.loads(raw.stdout);self.assertEqual(Q(data["height"]),h)
            for x in range(-2,3):
                for y in range(-2,3):
                    for z in range(-2,3):
                        p=(Q(x,2),Q(y,2),Q(z,2))
                        self.assertEqual(is_tie(ts,p,h),any(in_patch(p,q) for q in data["patches"]),(h,p))
if __name__=="__main__":unittest.main()
