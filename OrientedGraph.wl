(* ::Package:: *)

(* ::Title:: *)
(*Oriented Graph*)


(* ::Subtitle:: *)
(*Maxim Piskunov*)


BeginPackage["OrientedGraph`"];


Unprotect[VertexPort, GraphPort, OrientedGraph, OrientedGraphQ, OrientedGridGraph, WrappedAround, AdjacencyList, OrientedGraphIsomorphism];


VertexPort::usage = StringJoin @ {
	"VertexPort[\!\(\*StyleBox[\(v\), \"TI\"]\), \!\(\*StyleBox[\(p\), \"TI\"]\)] ",
	"yields the \!\(\*SuperscriptBox[StyleBox[\(p\), \"TI\"], th]\) port (that is an outlet to which edge connects) ",
	"of the \!\(\*SuperscriptBox[StyleBox[\(v\), \"TI\"], th]\) vertex of an oriented graph."
};


GraphPort::usage = StringJoin @ {
	"GraphPort[\!\(\*StyleBox[\(p\), \"TI\"]\)] ",
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


OrientedGridGraph::usage = StringJoin @ {
	"OrientedGridGraph[",
		"{\!\(\*StyleBox[\(m\), \"TI\"]\), \!\(\*StyleBox[\(n\), \"TI\"]\)}",
	"] gives the oriented grid graph with \!\(\*RowBox[{StyleBox[\"m\", \"TI\"], \"\[Times]\", StyleBox[\"n\", \"TI\"]}]\) vertices."
};


WrappedAround::usage = StringJoin @ {
	"WrappedAround is an option for OrientedGridGraph that specifies whether grid will be wrapped around in a torus or not."
};


OrientedGraphIsomorphism::usage = StringJoin @ {
	"FindOrietnedGraphIsomorphism[",
		"\!\(\*SubscriptBox[StyleBox[\(g\), \"TI\"], StyleBox[\(1\), \"TR\"]]\), ",
		"\!\(\*SubscriptBox[StyleBox[\(g\), \"TI\"], StyleBox[\(2\), \"TR\"]]\), ",
		"\!\(\*SubscriptBox[StyleBox[\(p\), \"TI\"], StyleBox[\(1\), \"TR\"]]\), ",
		"\!\(\*SubscriptBox[StyleBox[\(p\), \"TI\"], StyleBox[\(2\), \"TR\"]]\)",
	"] gives an isomorphism that maps the oriented graph \!\(\*SubscriptBox[StyleBox[\(g\), \"TI\"], StyleBox[\(1\), \"TR\"]]\) to ",
	"\!\(\*SubscriptBox[StyleBox[\(g\), \"TI\"], StyleBox[\(2\), \"TR\"]]\) by reordering vertices and ports, such that ",
	"the port \!\(\*SubscriptBox[StyleBox[\(p\), \"TI\"], StyleBox[\(1\), \"TR\"]]\) of \!\(\*SubscriptBox[StyleBox[\(g\), \"TI\"], StyleBox[\(1\), \"TR\"]]\) ",
	"corresponds to ",
	"\!\(\*SubscriptBox[StyleBox[\(p\), \"TI\"], StyleBox[\(2\), \"TR\"]]\) of \!\(\*SubscriptBox[StyleBox[\(g\), \"TI\"], StyleBox[\(2\), \"TR\"]]\)."
};


Begin["OrientedGraph`Private`"];


(* ::Section:: *)
(*Vertex Port*)


(* ::Subsection:: *)
(*SyntaxInformation*)


SyntaxInformation[VertexPort] = {"ArgumentsPattern" -> {_, _}};


(* ::Subsection:: *)
(*Consistency checks*)


VertexPort::argx = "VertexPort called with `1` arguments; 2 arguments are expected.";
VertexPort::intvert = "First argument in VertexPort[`1`, `2`] is not an integer.";
VertexPort::posvert = "First argument in VertexPort[`1`, `2`] is not positive.";
VertexPort::intport = "Second argument in VertexPort[`1`, `2`] is not an integer.";
VertexPort::posport = "Second argument in VertexPort[`1`, `2`] is not positive.";
VertexPort::toolargeport = "Only oriented graphs with 3 ports per vertex are supported.";


VertexPort[args___] := 0 /; Length @ {args} != 2 &&
	Message[VertexPort::argx, Length @ {args}]

VertexPort[vertexIndex_ ? (NumericQ[#] && !IntegerQ[#] &), portIndex_] := 0 /;
	Message[VertexPort::intvert, vertexIndex, portIndex]

VertexPort[vertexIndex_ ? (IntegerQ[#] && # <= 0 &), portIndex_] := 0 /;
	Message[VertexPort::posvert, vertexIndex, portIndex]

VertexPort[vertexIndex_ ? (IntegerQ[#] && # > 0 &), portIndex_ ? (NumericQ[#] && !IntegerQ[#] &)] := 0 /;
	Message[VertexPort::intport, vertexIndex, portIndex]

VertexPort[vertexIndex_ ? (IntegerQ[#] && # > 0 &), portIndex_ ? (IntegerQ[#] && # <= 0 &)] := 0 /;
	Message[VertexPort::posport, vertexIndex, portIndex]

VertexPort[vertexIndex_ ?(IntegerQ[#] && # > 0 &), portIndex_ ? (IntegerQ[#] && # > 3 &)] := 0 /;
	Message[VertexPort::toolargeport, 1, vertexIndex, portIndex]


(* ::Subsection:: *)
(*Standard Form*)


$PortColor[VertexPort[vertexIndex_, slotIndex_]] := ColorData[97, slotIndex]


VertexPort /: MakeBoxes[VertexPort[vertexIndex_Integer, portIndex_ ? (MatchQ[1|2|3])], StandardForm] /;
	vertexIndex > 0 :=
	With[{portLabel = ToBoxes @ Style[Subscript[vertexIndex, portIndex], $PortColor @ VertexPort[vertexIndex, portIndex]]},
		InterpretationBox[portLabel, VertexPort[vertexIndex, portIndex]]
	]


(* ::Section:: *)
(*Graph Port*)


(* ::Subsection:: *)
(*SyntaxInformation*)


SyntaxInformation[GraphPort] = {"ArgumentsPattern" -> {_}};


(* ::Subsection:: *)
(*Consistency checks*)


GraphPort::argx = "GraphPort called with `1` arguments; 1 argument is expected.";
GraphPort::int = "Argument `1` in GraphPort[`1`] is not an integer.";
GraphPort::pos = "Argument `1` in GraphPort[`1`] is not positive.";


GraphPort[args___] := 0 /; Length @ {args} != 1 &&
	Message[GraphPort::argx, Length @ {args}]

GraphPort[vertexIndex_ ? (NumericQ[#] && !IntegerQ[#] &)] := 0 /;
	Message[GraphPort::int, vertexIndex]

GraphPort[vertexIndex_ ? (IntegerQ[#] && # <= 0 &)] := 0 /;
	Message[GraphPort::pos, vertexIndex]


(* ::Subsection:: *)
(*Standard Form*)


GraphPort /: MakeBoxes[GraphPort[vertexIndex_Integer], StandardForm] /;
	vertexIndex > 0 :=
	With[{portLabel = ToBoxes @ Slot @ vertexIndex},
		InterpretationBox[portLabel, GraphPort[vertexIndex]]
	]


(* ::Section:: *)
(*Oriented Graph*)


(* ::Subsection:: *)
(*SyntaxInformation*)


SyntaxInformation[OrientedGraph] = {"ArgumentsPattern" -> {{___}}};


(* ::Subsection:: *)
(*$PortQ*)


$VertexPortQ[head_ ? (MatchQ[VertexPort | Subscript | List])[vertexIndex_Integer ? (# > 0 &), portIndex_ ? (MatchQ[1|2|3])]] := True

$VertexPortQ[args___] := False


$GraphPortQ[head_ ? (MatchQ[GraphPort | Slot]) @ x_Integer ? (# > 0 &)] := True

$GraphPortQ[args___] := False


$PortQ[args___] := $VertexPortQ[args] || $GraphPortQ[args]


(* ::Subsection:: *)
(*$ToCanonicalEdge*)


$ToCanonicalPort[port_] := port /. {
	Subscript | List -> VertexPort,
	Slot -> GraphPort
}


$ToCanonicalEdge[edge_] := $ToCanonicalPort /@ (edge[[1]] <-> edge[[2]])


(* ::Subsection:: *)
(*$PortList*)


$PortList[edges_List] := Flatten[List @@ # & /@ Map[$ToCanonicalPort, edges, {2}]]


$PortList[edges_List, head_] := Select[Head @ # == head &] @ $PortList[edges]


(* ::Subsection:: *)
(*$MaxVertexIndex and $MaxGraphPortIndex*)


$MaxVertexOrGraphPort[edges_List, head_] := Max[First /@ $PortList[edges, head], 0]


$MaxVertexIndex[edges_List] := $MaxVertexOrGraphPort[edges, VertexPort]


$MaxGraphPortIndex[edges_List] := $MaxVertexOrGraphPort[edges, GraphPort]


(* ::Subsection:: *)
(*$MissingPorts*)


$MissingPorts[edges_List] :=
	Complement[
		Join[
			VertexPort[#[[1]], #[[2]]] & /@ Tuples[{Range @ $MaxVertexIndex @ edges, Range @ 3}],
			GraphPort[#] & /@ Range @ $MaxGraphPortIndex @ edges
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


SyntaxInformation[OrientedGraphQ] = {"ArgumentsPattern" -> {_}};


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
		vertexAdjacentEdges = Flatten @ MapIndexed[Sort[#1 <-> VertexPort[#2[[1]], #2[[2]]]] &, List @@@ vertices, {2}],
		graphAdjacentEdges = MapIndexed[Sort[#1 <-> GraphPort[#2[[1]]]] &, graphPorts]
	},
	OrientedGraph @ Union @ Join[vertexAdjacentEdges, graphAdjacentEdges]
]


(* ::Subsection:: *)
(*Edge referencing*)


$VertexList[vertices_, graphPorts_][p_ ? $VertexPortQ] := vertices[[p[[1]], p[[2]]]]


$VertexList[vertices_, graphPorts_][p_ ? $GraphPortQ] := graphPorts[[p[[1]]]]


(* ::Section:: *)
(*$ModularIndex*)


(* ::Subsection:: *)
(*Plus*)


$ModularIndex /: Plus[$ModularIndex[i_Integer, m_Integer], n_Integer] := $ModularIndex[i + n, m]

$ModularIndex /: Plus[$ModularIndex[i_Integer], n_Integer] := $ModularIndex[i + n]


(* ::Subsection:: *)
(*$ToInteger*)


$ModularIndex /: $ToInteger[$ModularIndex[i_Integer, m_Integer]] := Mod[i - 1, m] + 1

$ModularIndex /: $ToInteger[$ModularIndex[i_Integer]] := i

$ToInteger[n_Integer] := n


(* ::Subsection:: *)
(*Part*)


$ModularIndex /: Part[obj_, $ModularIndex[i_Integer, m_Integer]] := Part[obj, Mod[i - 1, m] + 1]

$ModularIndex /: Part[obj_, $ModularIndex[i_Integer]] := Part[obj, Mod[i - 1, Length @ obj] + 1]


(* ::Section:: *)
(*OrientedGridGraph*)


(* ::Subsection:: *)
(*Options*)


Options[OrientedGridGraph] = {WrappedAround -> False};


(* ::Subsection:: *)
(*SyntaxInformation*)


SyntaxInformation[OrientedGridGraph] = {"ArgumentsPattern" -> {{_, _}, OptionsPattern[]}};


(* ::Subsection:: *)
(*Consistency checks*)


OrientedGridGraph::argx = "OrientedGridGraph called with `1` arguments; 1 argument is expected.";
OrientedGridGraph::nonopt = "Options expected (instead of `1`) beyond position 2 in `2`. An option must be a rule or a list of rules.";
OrientedGridGraph::lpn = "Argument `1` in `2` is not a list.";
OrientedGridGraph::dim = "Only two dimensional grids are supported.";
OrientedGridGraph::ilsmp = "List of positive integers expected at position 1 of `1`.";
OrientedGridGraph::neven = "Wrapped around grid should have even number of vertices in each direction instead of `1`.";


OrientedGridGraph[args___] := 0 /; Length @ {args} == 0 &&
	Message[OrientedGridGraph::argx, Length @ {args}]


OrientedGridGraph[arg_, opt__ ? (Not @* OptionQ)] := 0 /;
	Message[OrientedGridGraph::nonopt, Last @ {opt}, "OrientedGridGraph"[arg, opt]]


OrientedGridGraph[arg_ ? (MatchQ[Except[_List]]), opt___ ? OptionQ] := 0 /;
	Message[OrientedGridGraph::lpn, arg, "OrientedGridGraph"[arg, opt]]


OrientedGridGraph[arg_List ? (Length @ # != 2 &), opt___ ? OptionQ] := 0 /;
	Message[OrientedGridGraph::dim]


OrientedGridGraph[arg_List ? (Length @ # == 2 && AnyTrue[MatchQ[Except[_Integer | _Symbol]] @ # || # <= 0 &] @ # &), opt___ ? OptionQ] := 0 /;
	Message[OrientedGridGraph::ilsmp, "OrientedGridGraph"[arg, opt]]


OrientedGridGraph[{m_Integer ? (# > 0 &), n_Integer ? (# > 0 &)}, opts : OptionsPattern[]] := 0 /;
	FilterRules[{opts}, Options[OrientedGridGraph]] == {opts} && Not @ MatchQ[OptionValue[WrappedAround], True | False] &&
	Message[General::opttf, WrappedAround, OptionValue[WrappedAround]]


OrientedGridGraph[arg_List ? (Length @ # == 2 && AllTrue[MatchQ[_Integer] @ # && # > 0 &] @ # &), opts : OptionsPattern[]] := 0 /;
	FilterRules[{opts}, Options[OrientedGridGraph]] == {opts} && OptionValue[WrappedAround] === True && AnyTrue[Mod[#, 2] != 0 &] @ arg &&
	Message[OrientedGridGraph::neven, arg]


(* ::Subsection:: *)
(*OrientedGridGraph*)


$GridRegion[{{m1_Integer, m2_Integer}, {n1_Integer, n2_Integer}}] :=
	Flatten[{
		{#[[1]], #[[2]], 3, 1} <-> {#[[1]], #[[2]] - 1, 4, 1},
		{#[[1]], #[[2]], 3, 2} <-> {#[[1]] + 1, #[[2]], 1, 2},
		{#[[1]], #[[2]], 3, 3} <-> {#[[1]], #[[2]], 1, 3},
		{#[[1]], #[[2]], 2, 1} <-> {#[[1]], #[[2]], 1, 1},
		{#[[1]], #[[2]], 2, 2} <-> {#[[1]], #[[2]], 4, 2},
		{#[[1]], #[[2]], 2, 3} <-> {#[[1]] - 1, #[[2]], 4, 3}
	} & /@
	Tuples[Range @@ # & /@ {{m1, m2}, {n1, n2}}]]


$OrientedToroidalGridEdges[{m_Integer, n_Integer}] :=
	Map[
		VertexPort[2 n (#[[1]] - 1) + 4 (#[[2]] - 1) + #[[3]], #[[4]]] & @*
		({$ToInteger @ $ModularIndex[#[[1]], m / 2], $ToInteger @ $ModularIndex[#[[2]], n / 2], #[[3]], #[[4]]} &),
		$GridRegion[{{1, m / 2}, {1, n / 2}}],
		{2}
	]


$OrientedGridEdges[{m_Integer, n_Integer}] := Module[
	{
		graphPortIndex = 1
	},
	# /. {0 :> GraphPort[graphPortIndex++]} & @
	DeleteCases[0 <-> 0] @
	Map[
		If[
			#[[1]] < 1 || #[[1]] > Ceiling[m / 2] || #[[1]] > m / 2 && MatchQ[#[[3]], 3 | 4] ||
			#[[2]] < 1 || #[[2]] > Ceiling[n / 2] || #[[2]] > n / 2 && MatchQ[#[[3]], 2 | 4],
			0,
			VertexPort[
				2 n (#[[1]] - 1) +
				n If[MatchQ[#[[3]], 1 | 2], 0, 1] +
				2 (#[[2]] - 1) +
				If[MatchQ[#[[3]], 1 | 3], 0, 1] +
				1,
				#[[4]]
			]
		] &,
		$GridRegion[{{0, Ceiling[m / 2] + 1}, {0, Ceiling[n / 2] + 1}}],
		{2}
	]
]


OrientedGridGraph[{m_Integer ? (# > 0 &), n_Integer ? (# > 0 &)}, opts : OptionsPattern[]] :=
	OrientedGraph @
	If[OptionValue[WrappedAround],
		$OrientedToroidalGridEdges[{m, n}],
		$OrientedGridEdges[{m, n}]
	] /;
	MatchQ[OptionValue[WrappedAround], True | False] &&
	FilterRules[{opts}, Options[OrientedGridGraph]] == {opts} && If[OptionValue[WrappedAround], Mod[n, 2] == 0 && Mod[m, 2] == 0, True]


(* ::Section:: *)
(*$NextPort*)


$NextPort[p_ ? $VertexPortQ] := VertexPort[p[[1]], $ToInteger @ $ModularIndex[p[[2]] + 1, 3]]


$NextPort[p_ ? $GraphPortQ] := p


(* ::Section:: *)
(*$FaceWalk*)


$FaceWalk[g_$VertexList, p_ ? $PortQ] := $NextPort @ g @ p


(* ::Section:: *)
(*AdjacencyList*)


AdjacencyList[graph_ ? OrientedGraphQ] := {}


AdjacencyList[graph_ ? OrientedGraphQ, v_, arg___] := AdjacencyList[$ToGraph @ graph, $ToCanonicalPort @ v, arg]


(* ::Section:: *)
(*OrientedGraphIsomorphism*)


(* ::Subsection:: *)
(*SyntaxInformation*)


SyntaxInformation[OrientedGraphIsomorphism] = {"ArgumentsPattern" -> {_, _, _, _}};


(* ::Subsection:: *)
(*Consistancy checks*)


OrientedGraphIsomorphism::argx = "OrientedGraphIsomorphism called with `1` arguments; 4 arguments is expected.";
OrientedGraphIsomorphism::nonog = "Argument `1` in OrientedGraphIsomophism[`2`, `3`, `4`, `5`] is not an OrientedGraph";
OrientedGraphIsomorphism::nop = "Argument `1` in OrientedGraphIsomorphism[`2`, `3`, `4`, `5`] is not a port";


OrientedGraphIsomorphism[args___] := 0 /; Length @ {args} != 4 &&
	Message[OrientedGraphIsomorphism::argx, Length @ {args}]


OrientedGraphIsomorphism[g1_ ? (!OrientedGraphQ[#] && !(Head[#] === Symbol) &), g2_, p1_, p2_] := 0 /;
	Message[OrientedGraphIsomorphism::nonog, g1, g1, g2, p1, p2]


OrientedGraphIsomorphism[g1_ ? OrientedGraphQ, g2_ ? (!OrientedGraphQ[#] && !(Head[#] === Symbol) &), p1_, p2_] := 0 /;
	Message[OrientedGraphIsomorphism::nonog, g2, g1, g2, p1, p2]


OrientedGraphIsomorphism[g1_ ? OrientedGraphQ, g2_ ? OrientedGraphQ, p1_ ? (!$PortQ[#] && !(Head[#] === Symbol) &), p2_] := 0 /;
	Message[OrientedGraphIsomorphism::nop, p1, g1, g2, p1, p2]


OrientedGraphIsomorphism[g1_ ? OrientedGraphQ, g2_ ? OrientedGraphQ, p1_ ? $PortQ, p2_ ? (!$PortQ[#] && !(Head[#] === Symbol) &)] := 0 /;
	Message[OrientedGraphIsomorphism::nop, p2, g1, g2, p1, p2]


End[];


Attributes[VertexPort] = {ReadProtected};
Attributes[GraphPort] = {ReadProtected};
Attributes[OrientedGraph] = {ReadProtected};
Attributes[OrientedGraphQ] = {ReadProtected};
Attributes[OrientedGridGraph] = {ReadProtected};
Attributes[WrappedAround] = {ReadProtected};
Attributes[OrientedGraphIsomorphism] = {ReadProtected};


Protect[VertexPort, GraphPort, OrientedGraph, OrientedGraphQ, OrientedGridGraph, WrappedAround, AdjacencyList, OrientedGraphIsomorphism];


EndPackage[]
