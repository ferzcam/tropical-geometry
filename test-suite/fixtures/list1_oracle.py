#!/usr/bin/env python3
"""Independent exact integer oracle for TConvexHull3.list1.

Enumerate every supporting plane through a noncollinear input triple.
A point of a full-dimensional bounded hull is a vertex iff its incident
supporting normals span R^3. No production geometry code is used.
"""
from functools import reduce
from itertools import combinations
from math import gcd

POINTS = [
    (6,0,10), (5,1,6), (4,2,0), (3,3,3), (2,4,1), (1,5,0),
    (0,6,9), (5,0,7), (4,1,2), (3,2,4), (2,3,0), (1,4,3),
    (0,5,8), (4,0,4), (3,1,0), (2,2,2), (1,3,1), (0,4,4),
    (3,0,3), (2,1,1), (1,2,1), (0,3,3), (2,0,1),
]
EXPECTED = [
    (0,3,3), (0,4,4), (0,5,8), (0,6,9), (1,2,1), (1,5,0),
    (2,0,1), (3,1,0), (4,0,4), (4,2,0), (6,0,10),
]

def sub(a, b):
    return tuple(x-y for x,y in zip(a,b))

def dot(a, b):
    return sum(x*y for x,y in zip(a,b))

def cross(a, b):
    return (a[1]*b[2]-a[2]*b[1], a[2]*b[0]-a[0]*b[2],
            a[0]*b[1]-a[1]*b[0])

def spans_three(vectors):
    return any(dot(a, cross(b,c)) != 0 for a,b,c in combinations(vectors, 3))

def hull_vertices(points):
    assert spans_three([sub(p, points[0]) for p in points[1:]]), 'Need full-dimensional hull'
    planes = set()
    for a,b,c in combinations(points, 3):
        normal = cross(sub(b,a), sub(c,a))
        if not any(normal):
            continue
        offset = dot(normal,a)
        residuals = [dot(normal,p)-offset for p in points]
        if min(residuals) < 0 < max(residuals):
            continue
        if max(residuals) > 0:
            normal = tuple(-x for x in normal)
            offset = -offset
        divisor = reduce(gcd, normal)
        assert offset % divisor == 0
        plane = tuple(x//divisor for x in normal) + (offset//divisor,)
        assert all(dot(plane[:3],p) <= plane[3] for p in points)
        planes.add(plane)
    vertices = sorted(p for p in points if spans_three(
        [plane[:3] for plane in planes if dot(plane[:3],p) == plane[3]]))
    return vertices, planes

if __name__ == '__main__':
    vertices, planes = hull_vertices(POINTS)
    assert vertices == EXPECTED, (vertices, EXPECTED)
    print(f'{len(POINTS)} input points; {len(planes)} supporting facet planes; {len(vertices)} vertices')
    for vertex in vertices:
        print(vertex)
