(* ::Package:: *)

(* ::Title:: *)
(*Oriented Graph*)


(* ::Subtitle:: *)
(*Maxim Piskunov*)


BeginPackage["OrientedGraph`"];


Unprotect[OrientedVertexPort, OrientedGraphPort, OrientedGraph, OrientedGraphQ];


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


Begin["OrientedGraph`Private`"];


(* ::Section:: *)
(*Oriented Vertex Port*)


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

OrientedVertexPort[vertexIndex_ ? (NumericQ[#] && IntegerQ[#] && # <= 0 &), portIndex_] := 0 /;
	Message[OrientedVertexPort::posvert, vertexIndex, portIndex]

OrientedVertexPort[vertexIndex_ ? (IntegerQ[#] && # > 0 &), portIndex_ ? (NumericQ[#] && !IntegerQ[#] &)] := 0 /;
	Message[OrientedVertexPort::intport, vertexIndex, portIndex]

OrientedVertexPort[vertexIndex_ ? (IntegerQ[#] && # > 0 &), portIndex_ ? (NumericQ[#] && IntegerQ[#] && # <= 0 &)] := 0 /;
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
(*Consistency checks*)


OrientedGraphPort::argx = "OrientedGraphPort called with `1` arguments; 1 argument is expected.";
OrientedGraphPort::int = "Argument `1` in OrientedGraphPort[`1`] is not an integer.";
OrientedGraphPort::pos = "Argument `1` in OrientedGraphPort[`1`] is not positive.";


OrientedGraphPort[args___] := 0 /; Length @ {args} != 1 &&
	Message[OrientedGraphPort::argx, Length @ {args}]

OrientedGraphPort[vertexIndex_ ? (NumericQ[#] && !IntegerQ[#] &)] := 0 /;
	Message[OrientedGraphPort::int, vertexIndex]

OrientedGraphPort[vertexIndex_ ? (NumericQ[#] && IntegerQ[#] && # <= 0 &)] := 0 /;
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
(*$PortQ*)


$PortQ[head_ ? (MatchQ[OrientedVertexPort | Subscript | List])[vertexIndex_Integer ? (# > 0 &), portIndex_ ? (MatchQ[1|2|3])]] := True

$PortQ[head_ ? (MatchQ[OrientedGraphPort | Slot]) @ x_Integer ? (# > 0 &)] := True

$PortQ[arg___] := False


(* ::Subsection:: *)
(*$ToCanonicalEdge*)


$ToCanonicalPort[port_] := port /. {
	Subscript | List -> OrientedVertexPort,
	Slot -> OrientedGraphPort
}


$ToCanonicalEdge[edge_] := $ToCanonicalPort /@ (edge[[1]] <-> edge[[2]])


(* ::Subsection:: *)
(*Consistency checks*)


$PortsList[edges_List] := Flatten[List @@ # & /@ Map[$ToCanonicalPort, edges, {2}]]


$MaxVertexIndex[ports_List] := Max[ports[[All, 1]]]


$CompletePortsListQ[ports_] := Length @ Union[ports] == 3 $MaxVertexIndex @ ports


$CompleteOutgoingPortsListQ[ports_] := Length @ Union[ports] == $MaxVertexIndex @ ports


$CompleteEdgesListQ[edges_] := With[
	{
		portsList = $PortsList @ edges
	},
	$CompletePortsListQ[#] && DuplicateFreeQ[#] & @ Select[Head @ # == OrientedGraphPort &] @ portsList &&
	$CompleteOutgoingPortsListQ[#] && DuplicateFreeQ[#] & @ Select[Head @ # == OutgoingOrientedGraphPort &] @ portsList
]


OrientedGraph[edges_List] /; (
	AllTrue[
		MatchQ[Head[#], UndirectedEdge | List] &&
		AllTrue[$PortQ @ # &] @ # &
	] @ # && $CompleteEdgesListQ @ # & @ edges
) := Module[
	{
		canonicalEdges = $ToCanonicalEdge /@ edges,
		maxVertexIndex = $MaxVertexIndex @ Select[Head @ # == OrientedGraphPort &] @ $PortsList @ edges,
		directionsList
	},
	directionsList = (Sort @ Join[canonicalEdges, Reverse /@ canonicalEdges])[[All, 2]];
	OrientedGraph @ {Partition[#[[ ;; 3 maxVertexIndex]], 3], Partition[#[[3 maxVertexIndex + 1 ;; ]], 1]} & @ directionsList
]


End[];


Attributes[OrientedVertexPort] = {ReadProtected};
Attributes[OrientedGraphPort] = {ReadProtected};
Attributes[OrientedGraph] = {ReadProtected};
Attributes[OrientedGraphQ] = {ReadProtected};


Protect[OrientedVertexPort, OrientedGraphPort, OrientedGraph, OrientedGraphQ];


EndPackage[]
