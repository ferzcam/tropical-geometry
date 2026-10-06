#!/usr/bin/env python3
"""Recompute comparison evidence with independent exact Fraction arithmetic.

Supporting facets are found by exhaustive independent 2D/3D point subsets,
not by either Haskell hull implementation. This intentionally targets small
correctness fixtures; it is not part of the measured benchmark path.
"""
from fractions import Fraction as Q
from itertools import combinations, product
from math import gcd, lcm
import argparse
import json
from pathlib import Path


def vector(values):
    return tuple(Q(value) for value in values)


def dot(a, b):
    return sum(x*y for x,y in zip(a,b))


def canonical_facet(normal, bound):
    values = tuple(normal) + (bound,)
    scale = lcm(*(v.denominator for v in values))
    integers = tuple(int(v*scale) for v in values)
    divisor = gcd(*integers)
    if divisor == 0:
        raise ValueError("Zero supporting normal")
    return tuple(v//divisor for v in integers)


def supporting_facets(points):
    points = sorted(set(map(vector, points)))
    if not points or len(points[0]) not in (2,3):
        raise ValueError("Independent facet oracle supports nonempty 2D/3D clouds")
    dimension = len(points[0])
    if any(len(p) != dimension for p in points):
        raise ValueError("Ragged point cloud")
    result = set()
    for basis in combinations(points, dimension):
        differences = [tuple(b-a for a,b in zip(basis[0],p)) for p in basis[1:]]
        if dimension == 2:
            x,y = differences[0]
            normal = (y,-x)
        else:
            a,b = differences
            normal = (a[1]*b[2]-a[2]*b[1], a[2]*b[0]-a[0]*b[2], a[0]*b[1]-a[1]*b[0])
        if not any(normal):
            continue
        bound = dot(normal,basis[0])
        values = [dot(normal,p)-bound for p in points]
        if all(v <= 0 for v in values) and any(v < 0 for v in values):
            result.add(canonical_facet(normal,bound))
        elif all(v >= 0 for v in values) and any(v > 0 for v in values):
            result.add(canonical_facet(tuple(-v for v in normal),-bound))
    return result


def matrix_rank(rows):
    rows = [list(map(Q,row)) for row in rows]
    if not rows:
        return 0
    pivot = 0
    for col in range(len(rows[0])):
        found = next((i for i in range(pivot,len(rows)) if rows[i][col]),None)
        if found is None:
            continue
        rows[pivot],rows[found] = rows[found],rows[pivot]
        d = rows[pivot][col]
        rows[pivot] = [x/d for x in rows[pivot]]
        for i in range(pivot+1,len(rows)):
            factor = rows[i][col]
            rows[i] = [a-factor*b for a,b in zip(rows[i],rows[pivot])]
        pivot += 1
        if pivot == len(rows):
            break
    return pivot


def extreme_vertices(points, facets):
    points = sorted(set(map(vector,points)))
    dimension = len(points[0])
    return {p for p in points if matrix_rank([f[:-1] for f in facets if dot(f[:-1],p) == f[-1]]) == dimension}


def minimum_tie(terms, p):
    coefficients = {}
    for t in terms:
        key = int(t['x']),int(t['y'])
        value = Q(t['coefficient'])
        coefficients[key] = min(value,coefficients.get(key,value))
    values = [c+x*p[0]+y*p[1] for (x,y),c in coefficients.items()]
    return bool(values) and values.count(min(values)) >= 2


def edge_contains(edge, p):
    start = vector(edge['start'])
    direction = tuple(b-a for a,b in zip(start,vector(edge['end']))) if edge['kind'] == 'segment' else vector(edge['direction'])
    delta = tuple(b-a for a,b in zip(start,p))
    if delta[0]*direction[1] != delta[1]*direction[0]:
        return False
    axis = 0 if direction[0] else 1
    if not direction[axis]:
        raise ValueError('Zero-length edge')
    t = delta[axis]/direction[axis]
    return edge['kind'] == 'line' or (t >= 0 and (edge['kind'] == 'ray' or t <= 1))


def check_curve(terms, curve):
    errors = []
    edges = curve['edges']
    for x,y in product(range(-4,5),repeat=2):
        p = Q(x,2),Q(y,2)
        if minimum_tie(terms,p) != any(edge_contains(e,p) for e in edges):
            errors.append(f'Grid root disagreement at {p}')
    for edge in edges:
        start = vector(edge['start'])
        if edge['kind'] == 'segment':
            end = vector(edge['end'])
            direction = tuple(b-a for a,b in zip(start,end))
            parameters = [Q(0),Q(1,3),Q(1,2),Q(1)]
        else:
            direction = vector(edge['direction'])
            parameters = [Q(0),Q(1,3),Q(3)] + ([Q(-3)] if edge['kind'] == 'line' else [])
        for t in parameters:
            p = tuple(a+t*b for a,b in zip(start,direction))
            if not minimum_tie(terms,p):
                errors.append(f'Edge sample is not a root: {p}')
    for vertex in curve['vertices']:
        p = vector(vertex['point'])
        balance = [Q(0),Q(0)]
        for edge in edges:
            if edge['kind'] == 'line':
                continue
            start = vector(edge['start'])
            if edge['kind'] == 'ray':
                if start != p:
                    continue
                direction = vector(edge['direction'])
            else:
                end = vector(edge['end'])
                if p not in (start,end):
                    continue
                other = end if p == start else start
                direction = tuple(b-a for a,b in zip(p,other))
            scale = lcm(*(d.denominator for d in direction))
            integers = [int(d*scale) for d in direction]
            divisor = gcd(*integers)
            if not divisor:
                errors.append('Zero-length incident edge')
                continue
            for i in range(2):
                balance[i] += Q(integers[i],divisor)*Q(edge['weight'])
        if balance != [0,0]:
            errors.append(f'Unbalanced vertex {p}: {balance}')
    return errors


def primitive(direction, unoriented=False):
    direction = vector(direction)
    scale = lcm(*(v.denominator for v in direction))
    values = tuple(int(v*scale) for v in direction)
    divisor = gcd(*values)
    if not divisor:
        raise ValueError('Zero direction')
    values = tuple(v//divisor for v in values)
    return tuple(-v for v in values) if unoriented and values < (0,0) else values


def curve_signature(curve):
    vertices = tuple(sorted(vector(v['point'] if isinstance(v,dict) else v) for v in curve['vertices']))
    edges = []
    for e in curve['edges']:
        p = vector(e['start'])
        w = Q(e['weight'])
        if w.denominator != 1 or w <= 0:
            raise ValueError('Edge weights must be positive integers')
        if e['kind'] == 'segment':
            a,b = sorted([p,vector(e['end'])])
            edges.append(('segment',a,b,w))
        elif e['kind'] == 'ray':
            edges.append(('ray',p,primitive(e['direction']),w))
        elif e['kind'] == 'line':
            dx,dy = primitive(e['direction'],True)
            edges.append(('line',(dx,dy),dy*p[0]-dx*p[1],w))
        else:
            raise ValueError('Unknown edge kind')
    cells = tuple(sorted(tuple(sorted(map(vector,cell))) for cell in curve['cells']))
    return vertices,tuple(sorted(edges)),cells


def hull2_boundary(points):
    points = sorted(set(points))
    if len(points) <= 1:
        return points
    def half(xs):
        out = []
        for p in xs:
            while len(out) >= 2:
                a,b = out[-2:]
                cross = (b[0]-a[0])*(p[1]-a[1])-(b[1]-a[1])*(p[0]-a[0])
                if cross > 0:
                    break
                out.pop()
            out.append(p)
        return out
    return half(points)[:-1]+half(reversed(points))[:-1]


def validate_curve(terms,curve):
    # Adapt the comparison schema to the earlier exact grid/edge checks.
    adapted = dict(curve,vertices=[{'point':v['point'] if isinstance(v,dict) else v} for v in curve['vertices']])
    errors = check_curve(terms,adapted)
    signature = curve_signature(curve)
    vertices,edges,cells = signature
    for label,objects in [('vertices',vertices),('edges',edges),('cells',cells)]:
        if len(objects) != len(set(objects)):
            errors.append(f'Duplicate {label}')
    normalized = {}
    for term in terms:
        key = int(term['x']),int(term['y'])
        normalized[key] = min(Q(term['coefficient']),normalized.get(key,Q(term['coefficient'])))
    lifted = [tuple(map(Q,xy))+(c,) for xy,c in normalized.items()]
    if lifted and matrix_rank([tuple(a-b for a,b in zip(p,lifted[0])) for p in lifted[1:]]) == 3:
        lower = [f for f in supporting_facets(lifted) if f[2] < 0]
        expected_vertices = tuple(sorted((Q(f[0],f[2]),Q(f[1],f[2])) for f in lower))
        expected_cells = tuple(sorted(tuple(sorted(hull2_boundary([p[:2] for p in lifted if dot(f[:3],p)==f[3]]))) for f in lower))
        if vertices != expected_vertices:
            errors.append('Curve vertices disagree with independent lower supporting planes')
        if cells != expected_cells:
            errors.append('Subdivision cells disagree with independent lower supporting planes')
    expected_cells_from_vertices = []
    for p in vertices:
        values = {xy:c+xy[0]*p[0]+xy[1]*p[1] for xy,c in normalized.items()}
        minimum = min(values.values())
        active = [xy for xy,v in values.items() if v == minimum]
        boundary = hull2_boundary(active)
        if len(boundary) < 3:
            errors.append(f'Curve vertex has no two-dimensional dual cell: {p}')
        expected_cells_from_vertices.append(tuple(sorted(map(vector,boundary))))
    if cells != tuple(sorted(expected_cells_from_vertices)):
        errors.append('Cells do not match exact active terms at curve vertices')
    for edge in curve['edges']:
        p = vector(edge['start'])
        if edge['kind'] == 'segment':
            p = tuple((a+b)/2 for a,b in zip(p,vector(edge['end'])))
        elif edge['kind'] == 'ray':
            p = tuple(a+b for a,b in zip(p,vector(edge['direction'])))
        values = {xy:c+xy[0]*p[0]+xy[1]*p[1] for xy,c in normalized.items()}
        active = sorted(xy for xy,v in values.items() if v == min(values.values()))
        if len(active) < 2:
            errors.append('Edge interior has fewer than two active exponents')
        else:
            a,b = active[0],active[-1]
            expected_weight = gcd(abs(a[0]-b[0]),abs(a[1]-b[1]))
            if Q(edge['weight']) != expected_weight:
                errors.append('Edge weight disagrees with independent active-exponent lattice length')
    return signature,errors


def legacy_segments(curve):
    segments = []
    for edge in curve['edges']:
        start = vector(edge['start'])
        if edge['kind'] == 'segment':
            end = vector(edge['end'])
        elif edge['kind'] == 'ray':
            direction = primitive(edge['direction'])
            end = tuple(a+10*b for a,b in zip(start,direction))
        else:
            raise ValueError('Legacy API does not represent full lines')
        if any(x.denominator != 1 for x in start+end):
            raise ValueError('Legacy API cannot represent fractional vertices')
        segments.append(tuple(sorted([start,end])))
    return tuple(sorted(set(segments)))


def normalized_terms3(terms):
    normalized = {}
    for term in terms:
        exponent = tuple(int(term[k]) for k in ('x','y','z'))
        c = Q(term['coefficient'])
        normalized[exponent] = min(c,normalized.get(exponent,c))
    return normalized


def active_rank3(terms,point):
    values = {exponent:c+dot(exponent,point) for exponent,c in terms.items()}
    minimum = min(values.values())
    active = [e for e,v in values.items() if v == minimum]
    return matrix_rank([tuple(a-b for a,b in zip(e,active[0])) for e in active[1:]])


def solve_exact(rows,rhs):
    n = len(rows)
    rows = [list(map(Q,row))+[Q(b)] for row,b in zip(rows,rhs)]
    for column in range(n):
        pivot = next((i for i in range(column,n) if rows[i][column]),None)
        if pivot is None:
            return None
        rows[column],rows[pivot] = rows[pivot],rows[column]
        scale = rows[column][column]
        rows[column] = [x/scale for x in rows[column]]
        for i in range(n):
            if i != column:
                scale = rows[i][column]
                rows[i] = [a-scale*b for a,b in zip(rows[i],rows[column])]
    return tuple(row[-1] for row in rows)


def skeleton_vertices3(terms):
    vertices = set()
    for chosen in combinations(terms,4):
        first = chosen[0]
        rows = [tuple(a-b for a,b in zip(first,e)) for e in chosen[1:]]
        rhs = [terms[e]-terms[first] for e in chosen[1:]]
        point = solve_exact(rows,rhs)
        if point is not None:
            value = terms[first]+dot(first,point)
            if all(c+dot(e,point) >= value for e,c in terms.items()):
                vertices.add(point)
    return vertices


def skeleton_signature3(result):
    vertices = tuple(sorted(vector(p) for p in result['vertices']))
    if any(len(p) != 3 for p in vertices):
        raise ValueError('Skeleton vertices need three coordinates')
    edges = []
    for edge in result['edges']:
        start = vector(edge['start'])
        if len(start) != 3:
            raise ValueError('Skeleton edge starts need three coordinates')
        if edge['kind'] == 'segment':
            end = vector(edge['end'])
            if len(end) != 3 or start == end:
                raise ValueError('Invalid skeleton segment endpoint')
            a,b = sorted([start,end])
            edges.append(('segment',a,b))
        elif edge['kind'] in ('ray','line'):
            direction = primitive(edge['direction'])
            if len(direction) != 3:
                raise ValueError('Skeleton directions need three coordinates')
            if edge['kind'] == 'line':
                axis = next(i for i,d in enumerate(direction) if d)
                if direction[axis] < 0:
                    direction = tuple(-d for d in direction)
                t = -start[axis]/direction[axis]
                start = tuple(p+t*d for p,d in zip(start,direction))
            edges.append((edge['kind'],start,direction))
        else:
            raise ValueError('Unknown skeleton edge kind')
    return vertices,tuple(sorted(edges))


def edge_contains3(edge,p):
    start = vector(edge['start'])
    direction = tuple(b-a for a,b in zip(start,vector(edge['end']))) if edge['kind'] == 'segment' else vector(edge['direction'])
    axis = next((i for i,d in enumerate(direction) if d),None)
    if axis is None:
        raise ValueError('Zero skeleton direction')
    t = (p[axis]-start[axis])/direction[axis]
    if any(a+t*d != b for a,d,b in zip(start,direction,p)):
        return False
    return edge['kind'] == 'line' or (t >= 0 and (edge['kind'] == 'ray' or t <= 1))


def validate_skeleton3(input_terms,result):
    """Complete independent vertices; sampled edge validity and grid coverage.

    Complete edge-set equality is checked across methods, rather than claimed
    as an independent exhaustive edge oracle. This checks only the one-skeleton,
    not the two-dimensional faces of the ambient tropical hypersurface.
    """
    terms = normalized_terms3(input_terms)
    signature = skeleton_signature3(result)
    vertices,edges = signature
    problems = []
    if len(set(vertices)) != len(vertices) or len(set(edges)) != len(edges):
        problems.append('Duplicate skeleton vertices or edges')
    expected = skeleton_vertices3(terms)
    if set(vertices) != expected:
        problems.append(f'Skeleton vertices differ from independent four-term ties: missing={sorted(expected-set(vertices))}, extra={sorted(set(vertices)-expected)}')
    for p in vertices:
        if active_rank3(terms,p) < 3:
            problems.append(f'Skeleton vertex lacks rank-three active support: {p}')
    for edge in result['edges']:
        start = vector(edge['start'])
        if edge['kind'] == 'segment':
            end = vector(edge['end'])
            direction = tuple(b-a for a,b in zip(start,end))
            parameters = [Q(0),Q(1,3),Q(1,2),Q(2,3),Q(1)]
            if start not in expected or end not in expected:
                problems.append('Skeleton segment endpoint is not a true vertex')
        else:
            direction = vector(edge['direction'])
            parameters = [Q(0),Q(1,3),Q(1),Q(3),Q(10)]
            if edge['kind'] == 'line':
                parameters += [Q(-1),Q(-3),Q(-10)]
            elif start not in expected:
                problems.append('Skeleton ray origin is not a true vertex')
        for t in parameters:
            p = tuple(a+t*d for a,d in zip(start,direction))
            if active_rank3(terms,p) < 2:
                problems.append(f'Skeleton edge sample lacks rank-two minimum tie: {p}')
    for xyz in product(range(-2,3),repeat=3):
        p = tuple(Q(x,2) for x in xyz)
        if (active_rank3(terms,p) >= 2) != any(edge_contains3(e,p) for e in result['edges']):
            problems.append(f'Skeleton grid coverage disagreement at {p}')
    return signature,problems


def read_jsonl(path):
    with Path(path).open() as stream:
        return [json.loads(line) for line in stream if line.strip()]


def verify(fixtures,results):
    by_id = {f['case_id']:f for f in fixtures}
    if len(by_id) != len(fixtures):
        raise ValueError('Duplicate fixture case_id')
    report = {'fixtures':len(fixtures),'checked_results':0,'checked_hulls':0,'checked_curves':0,'checked_legacy':0,'checked_skeletons3':0,
              'unsupported':[],'skipped_fixtures':[],'errors':[],'mismatches':[],'cross_method_comparisons':0,
              'skeleton3_validation':'Complete four-term-tie vertex oracle; edge samples and finite grid; complete edge equality across methods. One-skeleton only, not full surface.'}
    seen = set()
    signatures = {}
    legacy_rows = []
    for row in results:
        case_id,method = row['case_id'],row['method']
        key = case_id,method
        if key in seen:
            report['errors'].append({'case_id':case_id,'method':method,'error':'Duplicate correctness result'})
            continue
        seen.add(key)
        if case_id not in by_id:
            report['errors'].append({'case_id':case_id,'method':method,'error':'Unknown fixture'})
            continue
        status = row.get('status','ok')
        if status == 'unsupported':
            report['unsupported'].append({'case_id':case_id,'method':method,'reason':row.get('error','No reason supplied')})
            continue
        if status != 'ok':
            report['errors'].append({'case_id':case_id,'method':method,'error':row.get('error',status)})
            continue
        fixture = by_id[case_id]
        result = row['result']
        if 'legacySegments' in result:
            legacy_rows.append(row)
            continue
        try:
            if fixture['kind'] in ('hull2','hull3'):
                points = list(map(vector,fixture['points']))
                dimension = len(points[0])
                if matrix_rank([tuple(a-b for a,b in zip(p,points[0])) for p in points[1:]]) < dimension:
                    raise ValueError('Full-dimensional facet comparison received rank-deficient fixture')
                expected = supporting_facets(points)
                actual_list = [canonical_facet(vector(f['normal']),Q(f['bound'])) for f in result['facets']]
                actual = set(actual_list)
                problems = []
                if len(actual) != len(actual_list):
                    problems.append('Duplicate facet inequality')
                if actual != expected:
                    problems.append(f'Independent facets differ: missing={sorted(expected-actual)}, extra={sorted(actual-expected)}')
                signature = tuple(sorted(actual))
                report['checked_hulls'] += 1
            elif fixture['kind'] == 'curve':
                signature,problems = validate_curve(fixture['terms'],result)
                report['checked_curves'] += 1
            elif fixture['kind'] == 'skeleton3':
                signature,problems = validate_skeleton3(fixture['terms'],result)
                report['checked_skeletons3'] += 1
            else:
                raise ValueError('Unknown fixture kind')
            report['checked_results'] += 1
            for problem in problems:
                report['mismatches'].append({'case_id':case_id,'method':method,'detail':problem})
            if case_id in signatures:
                previous_method,previous_signature = signatures[case_id]
                report['cross_method_comparisons'] += 1
                if signature != previous_signature:
                    report['mismatches'].append({'case_id':case_id,'method':method,'detail':f'Complete canonical output differs from {previous_method}'})
            else:
                signatures[case_id] = method,signature
        except (ValueError,KeyError,TypeError,ZeroDivisionError,IndexError) as error:
            report['errors'].append({'case_id':case_id,'method':method,'error':str(error)})
    for row in legacy_rows:
        case_id,method = row['case_id'],row['method']
        try:
            candidates = [r for r in results if r['case_id'] == case_id and r.get('status','ok') == 'ok' and 'edges' in r.get('result',{})]
            candidates.sort(key=lambda r: r['method'] != 'exact')
            if not candidates or case_id not in signatures:
                raise ValueError('Legacy regression has no independently checked canonical curve')
            expected = legacy_segments(candidates[0]['result'])
            actual = tuple(sorted(tuple(sorted(map(vector,segment))) for segment in row['result']['legacySegments']))
            if any(len(segment) != 2 or any(len(p) != 2 or any(c.denominator != 1 for c in p) for p in segment) for segment in actual):
                raise ValueError('Legacy segments must contain two integral 2D endpoints')
            report['checked_results'] += 1
            report['checked_legacy'] += 1
            if actual != expected:
                report['mismatches'].append({'case_id':case_id,'method':method,'detail':'Original legacy hypersurface segments differ from canonical exact curve rendered with length-10 primitive rays'})
        except (ValueError,KeyError,TypeError,ZeroDivisionError,IndexError) as error:
            report['errors'].append({'case_id':case_id,'method':method,'error':str(error)})
    for fixture in fixtures:
        if fixture['case_id'] not in signatures:
            captured = [r for r in results if r.get('case_id') == fixture['case_id']]
            if captured and all(r.get('status') == 'unsupported' for r in captured):
                report['skipped_fixtures'].append(fixture['case_id'])
            else:
                report['errors'].append({'case_id':fixture['case_id'],'error':'No verified supported output for fixture'})
        for method in fixture.get('methods',[]):
            if (fixture['case_id'],method) not in seen:
                report['errors'].append({'case_id':fixture['case_id'],'method':method,'error':'Missing correctness result'})
    report['passed'] = not report['errors'] and not report['mismatches'] and bool(report['checked_results'])
    return report


def self_test():
    square = [[0,0],[0,1],[1,0],[1,1]]
    expected = {(-1,0,0),(1,0,1),(0,-1,0),(0,1,1)}
    assert supporting_facets(square) == expected
    cube = list(product([0,1],repeat=3))
    cube_expected = set()
    for axis in range(3):
        for direction,bound in [(-1,0),(1,1)]:
            normal = [0,0,0]
            normal[axis] = direction
            cube_expected.add(tuple(normal)+(bound,))
    assert supporting_facets(cube) == cube_expected
    fixture = {'case_id':'square','kind':'hull2','points':square,'methods':['polar']}
    good = {'case_id':'square','method':'polar','result':{'facets':[{'normal':f[:-1],'bound':f[-1]} for f in expected]}}
    assert verify([fixture],[good])['passed']
    bad = dict(good,result={'facets':good['result']['facets'][:-1]})
    assert not verify([fixture],[bad])['passed']
    terms = [{'x':0,'y':0,'coefficient':'0'},{'x':1,'y':0,'coefficient':'0'},{'x':0,'y':1,'coefficient':'0'}]
    curve = {'vertices':[['0','0']],'edges':[{'kind':'ray','start':['0','0'],'direction':d,'weight':1} for d in [[1,0],[0,1],[-1,-1]]],'cells':[[[0,0],[1,0],[0,1]]]}
    assert not validate_curve(terms,curve)[1]
    assert len(legacy_segments(curve)) == 3
    assert ((Q(-10),Q(-10)),(Q(0),Q(0))) in legacy_segments(curve)
    corrupted = dict(curve,edges=curve['edges'][:-1])
    assert validate_curve(terms,corrupted)[1]
    corrupted_weight = dict(curve,edges=[dict(e,weight=2) for e in curve['edges']])
    assert validate_curve(terms,corrupted_weight)[1]
    terms3 = [{'x':x,'y':y,'z':z,'coefficient':'0'} for x,y,z in [(0,0,0),(1,0,0),(0,1,0),(0,0,1)]]
    skeleton = {'vertices':[[0,0,0]],'edges':[{'kind':'ray','start':[0,0,0],'direction':d} for d in [[1,0,0],[0,1,0],[0,0,1],[-1,-1,-1]]]}
    assert not validate_skeleton3(terms3,skeleton)[1]
    assert validate_skeleton3(terms3,dict(skeleton,edges=skeleton['edges'][:-1]))[1]
    assert validate_skeleton3(terms3,dict(skeleton,vertices=[]))[1]
    shifted3 = [dict(t,coefficient=str(-Q(t['x'],2)-Q(t['y'],3)-Q(t['z'],4))) for t in terms3]
    assert skeleton_vertices3(normalized_terms3(shifted3)) == {(Q(1,2),Q(1,3),Q(1,4))}
    print('Verifier self-checks passed: square/cube facets, missing facet, missing ray, incorrect weights, 3D simplex skeleton and corruptions.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--self-test',action='store_true')
    parser.add_argument('--fixtures')
    parser.add_argument('--results')
    parser.add_argument('--output')
    args = parser.parse_args()
    if args.self_test:
        self_test()
        return
    if not all([args.fixtures,args.results,args.output]):
        parser.error('--fixtures, --results, and --output are required unless --self-test is used')
    report = verify(read_jsonl(args.fixtures),read_jsonl(args.results))
    Path(args.output).write_text(json.dumps(report,indent=2)+'\n')
    print(json.dumps(report,indent=2))
    raise SystemExit(0 if report['passed'] else 1)


if __name__ == '__main__':
    main()
