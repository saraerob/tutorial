Let d = d(x,c)
if d > B:
P(x) += 1/d^f
else: 
P(x) += B^(f-g)/(2B-d)^g
rossmoDecay[p1_, p2_, bufferLength_, f_, g_, distance_] :=
With[{d = distance[p1, p2]},
If[d > bufferLength,
1/(d^f),
(bufferLength^(g - f))/(2 bufferLength - d)^g]];
makeRossmoFunction[sites_, buffer_, f_, g_] :=
Function[{x, y},
Apply[Plus,
Map[rossmoDecay[#,{x,y},buffer,f,g,ManhattanDistance] &,
sites]]];
Array[makeRossmoFunction[sites, 14, 1/3, 2/3], {60, 50}];
sites = {{20, 25}, {47, 10}, {55, 40}};
