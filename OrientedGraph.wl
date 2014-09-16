(* ::Package:: *)

(* ::Title:: *)
(*Oriented Graph*)


(* ::Subtitle:: *)
(*Maxim Piskunov*)


BeginPackage["OrientedGraph`"];


Unprotect[OrientedVertexPort, OrientedGraphPort, OrientedGraph, OrientedGraphQ, OrientedToroidalGridGraph];


OrientedVertexPort::usage = StringJoin @ {
	"OrientedVertexPort[\!\(\*StyleBox[\(v\), \"TI\"]\), \!\(\*StyleBox[\(p\), \"TI\"]\)] ",
	"yields the \!\(\*SuperscriptBox[StyleBox[\(p\), \"TI\"], th]\) port (that is an outlet to which edge connects) ",
	"of the \!\(\*SuperscriptBox[StyleBox[\(v\), \"TI\"], th]\) vertex of an oriented graph."
};


OrientedGraphPort::usage = StringJoin @ {
	"OrientedGraphPort[\!\(\*StyleBox[\(p\), \"TI\"]\)] ",
	" yields the \!\(\*SuperscriptBox[StyleBox[\(p\), \"TI\"], th]\) outgoing port (that is an outlet which is free) of an oriented graph."
};


OrientedGraph::usage = StringJoin @ {
	"OrientedGraph[",
		"{\!\(\*SubscriptBox[StyleBox[\(e\), \"TI\"], StyleBox[\(1\), \"TR\"]]\)",
		"\!\(\*SubscriptBox[StyleBox[\(e\), \"TI\"], StyleBox[\(2\), \"TR\"]]\), \[Ellipsis]}",
	"] yields a graph with edges \!\(\*SubscriptBox[StyleBox[\(e\), \"TI\"], StyleBox[\(i\), \"TI\"]]\)."
};


OrientedGraphQ::usage =
	"OrientedGraphQ[\!\(\*StyleBox[\(g\), \"TI\"]\)] yields True if \!\(\*StyleBox[\(g\), \"TI\"]\) is a valid OrientedGraph object and False otherwise.";


OrientedToroidalGridGraph::usage = StringJoin @ {
	"OrientedToroidalGridGraph[",
		"{\!\(\*StyleBox[\(m\), \"TI\"]\), \!\(\*StyleBox[\(n\), \"TI\"]\)}",
	"] gives the oriented hexagonal grid graph with \!\(\*RowBox[{StyleBox[\"m\", \"TI\"], \"\[Times]\", StyleBox[\"n\", \"TI\"]}]\) vertices wrapped around in a torus."
};


Begin["OrientedGraph`Private`"];


(* ::Section:: *)
(*Oriented Vertex Port*)


(* ::Subsection:: *)
(*SyntaxInformation*)


SyntaxInformation[OrientedVertexPort] = {"ArgumentsPattern" -> {_, _}};


(* ::Subsection:: *)
(*Consistency checks*)


OrientedVertexPort::argx = "OrientedVertexPort called with `1` arguments; 2 arguments are expected.";
OrientedVertexPort::intvert = "First argument in OrientedVertexPort[`1`, `2`] is not an integer.";
OrientedVertexPort::posvert = "First argument in OrientedVertexPort[`1`, `2`] is not positive.";
OrientedVertexPort::intport = "Second argument in OrientedVertexPort[`1`, `2`] is not an integer.";
OrientedVertexPort::posport = "Second argument in OrientedVertexPort[`1`, `2`] is not positive.";
OrientedVertexPort::toolargeport = "Only oriented graphs with 3 ports per vertex are supported.";


OrientedVertexPort[args___] := 0 /; Length @ {args} != 2 &&
	Message[OrientedVertexPort::argx, Length @ {args}]

OrientedVertexPort[vertexIndex_ ? (NumericQ[#] && !IntegerQ[#] &), portIndex_] := 0 /;
	Message[OrientedVertexPort::intvert, vertexIndex, portIndex]

OrientedVertexPort[vertexIndex_ ? (IntegerQ[#] && # <= 0 &), portIndex_] := 0 /;
	Message[OrientedVertexPort::posvert, vertexIndex, portIndex]

OrientedVertexPort[vertexIndex_ ? (IntegerQ[#] && # > 0 &), portIndex_ ? (NumericQ[#] && !IntegerQ[#] &)] := 0 /;
	Message[OrientedVertexPort::intport, vertexIndex, portIndex]

OrientedVertexPort[vertexIndex_ ? (IntegerQ[#] && # > 0 &), portIndex_ ? (IntegerQ[#] && # <= 0 &)] := 0 /;
	Message[OrientedVertexPort::posport, vertexIndex, portIndex]

OrientedVertexPort[vertexIndex_ ?(IntegerQ[#] && # > 0 &), portIndex_ ? (IntegerQ[#] && # > 3 &)] := 0 /;
	Message[OrientedVertexPort::toolargeport, 1, vertexIndex, portIndex]


(* ::Subsection:: *)
(*Standard Form*)


$PortColor[OrientedVertexPort[vertexIndex_, slotIndex_]] := ColorData[97, slotIndex]


OrientedVertexPort /: MakeBoxes[OrientedVertexPort[vertexIndex_Integer, portIndex_ ? (MatchQ[1|2|3])], StandardForm] /;
	vertexIndex > 0 :=
	With[{portLabel = ToBoxes @ Style[vertexIndex, $PortColor @ OrientedVertexPort[vertexIndex, portIndex]]},
		InterpretationBox[portLabel, OrientedVertexPort[vertexIndex, portIndex]]
	]


(* ::Section:: *)
(*Oriented Graph Port*)


(* ::Subsection:: *)
(*SyntaxInformation*)


SyntaxInformation[OrientedGraphPort] = {"ArgumentsPattern" -> {_}};


(* ::Subsection:: *)
(*Consistency checks*)


OrientedGraphPort::argx = "OrientedGraphPort called with `1` arguments; 1 argument is expected.";
OrientedGraphPort::int = "Argument `1` in OrientedGraphPort[`1`] is not an integer.";
OrientedGraphPort::pos = "Argument `1` in OrientedGraphPort[`1`] is not positive.";


OrientedGraphPort[args___] := 0 /; Length @ {args} != 1 &&
	Message[OrientedGraphPort::argx, Length @ {args}]

OrientedGraphPort[vertexIndex_ ? (NumericQ[#] && !IntegerQ[#] &)] := 0 /;
	Message[OrientedGraphPort::int, vertexIndex]

OrientedGraphPort[vertexIndex_ ? (IntegerQ[#] && # <= 0 &)] := 0 /;
	Message[OrientedGraphPort::pos, vertexIndex]


(* ::Subsection:: *)
(*Standard Form*)


OrientedGraphPort /: MakeBoxes[OrientedGraphPort[vertexIndex_Integer], StandardForm] /;
	vertexIndex > 0 :=
	With[{portLabel = ToBoxes @ Slot @ vertexIndex},
		InterpretationBox[portLabel, OrientedGraphPort[vertexIndex]]
	]


(* ::Section:: *)
(*Oriented Graph*)


(* ::Subsection:: *)
(*SyntaxInformation*)


SyntaxInformation[OrientedGraph] = {"ArgumentsPattern" -> {{___}}};


(* ::Subsection:: *)
(*$PortQ*)


$VertexPortQ[head_ ? (MatchQ[OrientedVertexPort | Subscript | List])[vertexIndex_Integer ? (# > 0 &), portIndex_ ? (MatchQ[1|2|3])]] := True

$VertexPortQ[args___] := False


$GraphPortQ[head_ ? (MatchQ[OrientedGraphPort | Slot]) @ x_Integer ? (# > 0 &)] := True

$GraphPortQ[args___] := False


$PortQ[args___] := $VertexPortQ[args] || $GraphPortQ[args]


(* ::Subsection:: *)
(*$ToCanonicalEdge*)


$ToCanonicalPort[port_] := port /. {
	Subscript | List -> OrientedVertexPort,
	Slot -> OrientedGraphPort
}


$ToCanonicalEdge[edge_] := $ToCanonicalPort /@ (edge[[1]] <-> edge[[2]])


(* ::Subsection:: *)
(*$PortList*)


$PortList[edges_List] := Flatten[List @@ # & /@ Map[$ToCanonicalPort, edges, {2}]]


$PortList[edges_List, head_] := Select[Head @ # == head &] @ $PortList[edges]


(* ::Subsection:: *)
(*$MaxVertexIndex and $MaxGraphPortIndex*)


$MaxVertexOrGraphPort[edges_List, head_] := Max[First /@ $PortList[edges, head], 0]


$MaxVertexIndex[edges_List] := $MaxVertexOrGraphPort[edges, OrientedVertexPort]


$MaxGraphPortIndex[edges_List] := $MaxVertexOrGraphPort[edges, OrientedGraphPort]


(* ::Subsection:: *)
(*$MissingPorts*)


$MissingPorts[edges_List] :=
	Complement[
		Join[
			OrientedVertexPort[#[[1]], #[[2]]] & /@ Tuples[{Range @ $MaxVertexIndex @ edges, Range @ 3}],
			OrientedGraphPort[#] & /@ Range @ $MaxGraphPortIndex @ edges
		],
		$PortList @ edges
	]


(* ::Subsection:: *)
(*$EdgesQ*)


$EdgesQ[edges_] :=
	AllTrue[
		MatchQ[Head[#], UndirectedEdge | List] &&
		AllTrue[$PortQ @ # &] @ # &
	] @ edges


$CompleteEdgesQ[edges_] :=
	$EdgesQ @ edges && DuplicateFreeQ @ $PortList @ edges && $MissingPorts[edges] == {}


(* ::Subsection:: *)
(*Consistency checks*)


OrientedGraph::argx = "OrientedGraph called with `1` arguments; 1 argument is expected.";
OrientedGraph::dir = "Directed oriented graphs are not supported.";
OrientedGraph::dup = "Port `1` in OrientedGraph[`2`] appears multiple times.";
OrientedGraph::mport = "Port `1` in OrientedGraph[`2`] is missing.";


OrientedGraph[args___] := 0 /; Length @ {args} != 1 &&
	Message[OrientedGraph::argx, Length @ {args}]


OrientedGraph[edges_List ? (AnyTrue[MatchQ[Head[#], DirectedEdge | Rule] &])] := 0 /; Message[OrientedGraph::dir]


OrientedGraph[edges_List] := 0 /;
	$EdgesQ @ edges && !DuplicateFreeQ @ $PortList @ edges &&
	Message[OrientedGraph::dup, First @ Commonest @ $PortList @ edges, $ToCanonicalEdge /@ edges]


OrientedGraph[edges_List] := 0 /; 
	$EdgesQ @ edges && DuplicateFreeQ @ $PortList @ edges && $MissingPorts[edges] != {} &&
	Message[OrientedGraph::mport, First @ $MissingPorts @ edges, $ToCanonicalEdge /@ edges]


OrientedGraph[edges_List] := OrientedGraph[$ToCanonicalEdge /@ edges] /;
	$CompleteEdgesQ @ edges && edges =!= $ToCanonicalEdge /@ edges


(* ::Section:: *)
(*OrientedGraphQ*)


SyntaxInformation[OrientedGraphQ] = {"ArgumentsPattern" -> {_}}


OrientedGraphQ::argx = "OrientedGraphQ called with `1` arguments; 1 argument is expected.";


OrientedGraphQ[args___] := 0 /; Length @ {args} != 1 &&
	Message[OrientedGraphQ::argx, Length @ {args}]


OrientedGraphQ[arg_ ? (Head[#] != OrientedGraph &)] := False


OrientedGraphQ[OrientedGraph[edges_List ? $CompleteEdgesQ]] := True


OrientedGraphQ[arg_] := False


(* ::Section:: *)
(*$ToGraph*)


$ToGraph[OrientedGraph[edges_]] := Graph[
	Map[If[$VertexPortQ @ #, First @ #, #] &, edges, {2}]
]


(* ::Section:: *)
(*$VertexList*)


(* ::Subsection:: *)
(*OrientedGraph -> $VertexList*)


$VertexList[g_ ? OrientedGraphQ] := Module[
	{
		canonicalEdges = $ToCanonicalEdge /@ g[[1]],
		maxVertexIndex = $MaxVertexIndex @ g[[1]],
		directionsList
	},
	directionsList = (Sort @ Join[canonicalEdges, Reverse /@ canonicalEdges])[[All, 2]];
	$VertexList[$Vertex @@@ Partition[#[[- 3 maxVertexIndex ;; ]], 3], #[[ ;; - 3 maxVertexIndex - 1]]] & @ directionsList
]


(* ::Subsection:: *)
(*$VertexList -> OrientedGraph*)


OrientedGraph[$VertexList[vertices_, graphPorts_]] := Module[
	{
		vertexAdjacentEdges = Flatten @ MapIndexed[Sort[#1 <-> OrientedVertexPort[#2[[1]], #2[[2]]]] &, List @@@ vertices, {2}],
		graphAdjacentEdges = MapIndexed[Sort[#1 <-> OrientedGraphPort[#2[[1]]]] &, graphPorts]
	},
	Union @ Join[vertexAdjacentEdges, graphAdjacentEdges]
]


(* ::Subsection:: *)
(*Edge referencing*)


$VertexList[vertices_, graphPorts_][p_ ? $VertexPortQ] := vertices[[p[[1]], p[[2]]]]


$VertexList[vertices_, graphPorts_][p_ ? $GraphPortQ] := graphPorts[[p[[1]]]]


(* ::Section:: *)
(*$ModularIndex*)


(* ::Subsection:: *)
(*$ToInteger*)


$ModularIndex /: $ToInteger[$ModularIndex[i_Integer, m_Integer]] := Mod[i - 1, m] + 1

$ModularIndex /: $ToInteger[$ModularIndex[i_Integer]] := i


(* ::Subsection:: *)
(*Part*)


$ModularIndex /: Part[obj_, $ModularIndex[i_Integer, m_Integer]] := Part[obj, Mod[i - 1, m] + 1]

$ModularIndex /: Part[obj_, $ModularIndex[i_Integer]] := Part[obj, Mod[i - 1, Length @ obj] + 1]


(* ::Section:: *)
(*OrientedToroidalGridGraph*)


(* ::Subsection:: *)
(*Consistency checks*)


OrientedToroidalGridGraph::argx = "OrientedToroidalGridGraph called with `1` arguments; 1 argument is expected.";
OrientedToroidalGridGraph::lpn = "Argument `1` in OrientedToroidalGridGraph[`1`] is not a list.";
OrientedToroidalGridGraph::dim = "Only two dimensional grids are supported.";
OrientedToroidalGridGraph::ilsmp = "List of positive even integers expected at position 1 of OrientedToroidalGridGraph[`1`].";


OrientedToroidalGridGraph[args___] := 0 /; Length @ {args} != 1 &&
	Message[OrientedToroidalGridGraph::argx, Length @ {args}]


OrientedToroidalGridGraph[arg_ ? (MatchQ[Except[_List]])] := 0 /;
	Message[OrientedToroidalGridGraph::lpn, arg]


OrientedToroidalGridGraph[arg_List ? (Length @ # != 2 &)] := 0 /;
	Message[OrientedToroidalGridGraph::dim]


OrientedToroidalGridGraph[arg_List ? (Length @ # == 2 && AnyTrue[MatchQ[Except[_Integer | _Symbol]] @ # || # <= 0 || Mod[#, 2] != 0 &] @ # &)] := 0 /;
	Message[OrientedToroidalGridGraph::ilsmp, arg]


(* ::Subsection:: *)
(*OrientedGridGraph*)


OrientedToroidalGridGraph[{m_Integer ? (# > 0 && Mod[#, 2] == 0 &), n_Integer ? (# > 0 && Mod[#, 2] == 0 &)}] :=
	OrientedGraph @ Map[
		Function[{i, j, type, port},
			OrientedVertexPort[2 n (i - 1) + 4 (j - 1) + type, port]
		] @@ # &,
		Flatten[{
			{#[[1]], #[[2]], 1, 1} <-> {#[[1]], $ToInteger @ $ModularIndex[#[[2]] - 1, n / 2], 4, 1},
			{#[[1]], #[[2]], 1, 2} <-> {$ToInteger @ $ModularIndex[#[[1]] + 1, m / 2], #[[2]], 2, 2},
			{#[[1]], #[[2]], 1, 3} <-> {#[[1]], #[[2]], 2, 3},
			{#[[1]], #[[2]], 3, 1} <-> {#[[1]], #[[2]], 2, 1},
			{#[[1]], #[[2]], 3, 2} <-> {#[[1]], #[[2]], 4, 2},
			{#[[1]], #[[2]], 3, 3} <-> {$ToInteger @ $ModularIndex[#[[1]] - 1, m / 2], #[[2]], 4, 3}
		} & /@
		Tuples[Range /@ {m / 2, n / 2}]],
		{2}
	]


(* ::Section:: *)
(*$NextPort*)


$NextPort[p_ ? $VertexPortQ] := OrientedVertexPort[p[[1]], $ToInteger @ $ModularIndex[p[[2]] + 1, 3]]


$NextPort[p_ ? $GraphPortQ] := p


End[];


Attributes[OrientedVertexPort] = {ReadProtected};
Attributes[OrientedGraphPort] = {ReadProtected};
Attributes[OrientedGraph] = {ReadProtected};
Attributes[OrientedGraphQ] = {ReadProtected};
Attributes[OrientedToroidalGridGraph] = {ReadProtected};


Protect[OrientedVertexPort, OrientedGraphPort, OrientedGraph, OrientedGraphQ, OrientedToroidalGridGraph];


EndPackage[]
