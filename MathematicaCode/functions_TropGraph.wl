(* ::Package:: *)

(* ::Section:: *)
(*Functions to read output from Haskell*)


lowerFaces[name_] := 
 Map[ToExpression[StringSplit[#1, ","]] &, 
  StringSplit[Import[StringJoin["LF_", name, ".txt"], "Lines"], ";"]]
hyperPlanes[name_] := 
 Map[ToExpression[StringSplit[#1, ","]] &, 
  Import[StringJoin["hyp_", name, ".txt"], "Lines"]]
bVec[name_] := 
 Map[ToExpression[#1] &, 
  Import[StringJoin["b_", name, ".txt"], "Lines"]]
extremalVert[name_] := 
 Map[ToExpression[StringSplit[#1, ","]] &, 
  StringSplit[Import[StringJoin["vert_", name, ".txt"]], ";"]]


(* ::Section:: *)
(*Plot Cones and Regions enclosed in HyperPlanes*)


polygonHyp[H0_, b_, center_, \[Lambda]_] := 
  polygonHyp[H0, b, center, \[Lambda]] = Module[{H},
    H = H0.{x1, x2, x3};
    RegionPlot3D[
     And @@ Table[H[[i]] <= b[[i]] - 0.01, {i, 1, Length[H]}], {x1, 
      center[[1]] - \[Lambda], center[[1]] + \[Lambda]}, {x2, 
      center[[2]] - \[Lambda], center[[2]] + \[Lambda]}, {x3, 
      center[[3]] - \[Lambda], center[[3]] + \[Lambda]}, 
     MaxRecursion -> 10, PlotPoints -> 60, Mesh -> 0, 
     BoundaryStyle -> Black, PlotStyle -> Opacity[0.8], 
     ColorFunction -> "Pastel", Boxed -> True, 
     AxesLabel -> {"X", "Y", "Z"}, AxesStyle -> Black, 
     Ticks -> Automatic,  BaseStyle -> {FontSize -> 20, 
             FontFamily -> "Latin Modern Roman", 
                   Black, Bold}]
    ];
coneHyp[H0_, b_, center_, \[Lambda]_] := 
 coneHyp[H0, b, center, \[Lambda]] = Module[{H}, H = H0.{x1, x2, x3};
   RegionPlot3D[
    And @@ Table[H[[i]] <= b[[i]], {i, 1, Length[H]}], 
    {x1, center[[1]] - \[Lambda], center[[1]] + \[Lambda]}, 
    {x2, center[[2]] - \[Lambda], center[[2]] + \[Lambda]}, 
    {x3, center[[3]] - \[Lambda], center[[3]] + \[Lambda]}, 
    MaxRecursion -> 10, PlotPoints -> 60, BoundaryStyle -> Black, 
    PlotStyle -> {{Black, Opacity[0.3]}}, Mesh -> None, Boxed -> True,
     AxesLabel -> {"X", "Y", "Z"}, AxesStyle -> Black, 
    Ticks -> Automatic, 
    BaseStyle -> {FontSize -> 20, FontFamily -> "Latin Modern Roman", Black, Bold} 
    ]
 ];
polWithCones[H0_, b0_, conH_, vert_, center_, \[Lambda]_] := 
  Module[{pol, cones, zip},
   pol = polygonHyp[H0, b0, center, \[Lambda]];
   zip = Table[
    {conH[[i]], 
    conH[[i]].vert[[i]]}, 
    {i, 1, Length[conH]}
    ];
   cones = Map[
    coneHyp[#1[[1]], #1[[2]], center, \[Lambda]] &, 
    zip];
   Show[pol, cones]
   ];
facet[pts_, color_, opc_] := 
 Module[
 {chull}, 
  chull = ConvexHullMesh[pts];
  Graphics3D[
   GraphicsComplex[
    MeshCoordinates[chull], 
    {PointSize[0.015], 
    Black, Thick, 
    MeshCells[chull, 0],
    Black, Thickness[0.006], 
    MeshCells[chull, 1], 
    Opacity[opc], 
    color, 
    MeshCells[chull, 2]}
    ], 
   Boxed -> False]]
conv[pts_, color_, opc_] := 
 Module[{chull}, chull = ConvexHullMesh[pts];
  Graphics3D[
   GraphicsComplex[
    MeshCoordinates[chull], 
    {PointSize[0.002],
     Black, 
     Thick, 
     MeshCells[chull, 0], 
     Opacity[0], 
     Thickness[0], 
     MeshCells[chull, 1], 
     Opacity[opc], 
     color, 
     MeshCells[chull, 2]}
     ], 
   Boxed -> 
    False]](*HighlightMesh[ConvexHullMesh[pts,MeshCellStyle\[Rule]{\
color}],{Style[0,Directive[PointSize[0.015],Black,Thick]],Style[1,\
Black,Thick]}];*)


(* ::Section:: *)
(*Plots Subdivision with Convex Hull and Hyperplanes*)


plotSubdiv[subdiv_, evert_] := Module[{zpLF, zpProj},
      zpLF = 
        Table[{subdiv[[i]], 
            ColorData["BlueGreenYellow"][i/(Length[subdiv]*2)]}, 
            {i, 1, Length[subdiv]}];
      zpProj = 
        Table[{Map[proj[#1] &, subdiv[[i]]], 
            ColorData["BlueGreenYellow"][i/(Length[subdiv]*2)]}, 
            {i, 1, Length[subdiv]}];
      Show[
        Map[facet[#1[[1]], #1[[2]], 0.9] &, zpProj],
        conv[evert, ColorData["Pastel"][0.2], 0.2], 
        Map[facet[#1[[1]], #1[[2]], 0.9] &, zpLF], 
        Axes -> True,
        AxesStyle-> Black,
        AxesLabel -> {"X", "Y", "Z"}, BaseStyle -> {FontSize -> 20, 
     FontFamily -> "Latin Modern Roman", Black, Bold} ,
         GridBox -> {1, 1, 1} ]];
plotSubdivHyp[subdiv_, H_, b_, center_, \[Lambda]_] := 
  Module[{zpLF, zpProj, surf},
    surf = polygonHyp[H, b, center, \[Lambda]];
    zpLF = Table[{subdiv[[i]], 
          ColorData["BlueGreenYellow"][i/(Length[subdiv]*2)]}, 
          {i,1,Length[subdiv]}];
    zpProj = 
      Table[{Map[proj[#1] &, subdiv[[i]]], 
          ColorData["BlueGreenYellow"][i/(Length[subdiv]*2)]}, 
          {i, 1, Length[subdiv]}];
    Show[
      Map[facet[#1[[1]], #1[[2]], 0.9] &, zpProj], surf, 
      Map[facet[#1[[1]], #1[[2]], 0.9] &, zpLF],
      Axes -> True,
        AxesStyle-> Black,
        AxesLabel -> {"X", "Y", "Z"}, BaseStyle -> {FontSize -> 20, 
     FontFamily -> "Latin Modern Roman", Black, Bold} ,
         GridBox -> {1, 1, 1}
      ]
     ]


(* ::Section:: *)
(*Extras*)


proj[pt_] := {pt[[1]], pt[[2]], 0};
sph[x_, y_, z_, x0_, y0_, z0_, r_] := (x - x0)^2 + (y - y0)^2 + (z - z0)^2 - r
ptPlot[x0_, y0_, z0_, \[Lambda]_, color_] := 
	ContourPlot3D[
		sph[x, y, z, x0, y0, z0, 0.02] == 0, {x, -\[Lambda], \[Lambda]}, {y, -\[Lambda], \[Lambda]}, 
		{z, -\[Lambda], \[Lambda]}, ContourStyle -> color, 
		Mesh -> None]
rayPlot[x0_, y0_, z0_, vert_, \[Lambda]_] := 
	ParametricPlot3D[vert + t*{x0, y0, z0},
		{t, 0, \[Lambda]},
		{s, -\[Lambda], \[Lambda]}, 
		PlotStyle -> {{Red, Thick}}, 
		BoundaryStyle -> {{Red, Thickness[0.008]}}
		]
