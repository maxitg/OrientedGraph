(* ::Package:: *)

(* ::Title:: *)
(*Oriented Graph*)


(* ::Subtitle:: *)
(*Maxim Piskunov*)


BeginPackage["OrientedGraph`"];


Unprotect[OrientedGraphPort, OrientedGraph, OutgoingOrientedGraphPort];


OrientedGraphPort::usage = StringJoin @ Riffle[{
	"OrientedGraphPort[v, i] yields the i-th port (that is an outlet to which edge connects) of the v-th vertex of an oriented graph."
}, "\n"];


OutgoingOrientedGraphPort::usage = StringJoin @ Riffle[{
	"OutgoingOrientedGraphPort[i] yields the i-th outgoing port (that is an outlet which is free) of an oriented graph."
}, "\n"];


OrientedGraph::usage = StringJoin @ Riffle[{
	"OrientedGraph[{\!\(\*SubscriptBox[\(e\), \(1\)]\), \!\(\*SubscriptBox[\(e\), \(2\)]\), \[Ellipsis]}] yields a graph with edges \!\(\*SubscriptBox[\(e\), \(i\)]\)."
}, "\n"];

}, "\n"];


Begin["OrientedGraph`Private`"];


(* ::Section:: *)
(*Oriented Graph Port*)


(* ::Subsection:: *)
(*Consistency checks*)


OrientedGraphPort::argx = "OrientedGraphPort called with `1` arguments; 2 arguments are expected.";
OrientedGraphPort::intvert = "First argument in OrientedGraphPort[`1`, `2`] is not an integer.";
OrientedGraphPort::posvert = "First argument in OrientedGraphPort[`1`, `2`] is not positive.";
OrientedGraphPort::intport = "Second argument in OrientedGraphPort[`1`, `2`] is not an integer.";
OrientedGraphPort::posport = "Second argument in OrientedGraphPort[`1`, `2`] is not positive.";
OrientedGraphPort::toolargeport = "Only oriented graphs with 3 ports per vertex are supported.";


OrientedGraphPort[args___] := 0 /; Length @ {args} != 2 &&
	Message[OrientedGraphPort::argx, Length @ {args}]

OrientedGraphPort[vertexIndex_ ? (NumericQ[#] && !IntegerQ[#] &), portIndex_] := 0 /;
	Message[OrientedGraphPort::intvert, vertexIndex, portIndex]

OrientedGraphPort[vertexIndex_ ? (NumericQ[#] && IntegerQ[#] && # <= 0 &), portIndex_] := 0 /;
	Message[OrientedGraphPort::posvert, vertexIndex, portIndex]

OrientedGraphPort[vertexIndex_ ? (IntegerQ[#] && # > 0 &), portIndex_ ? (NumericQ[#] && !IntegerQ[#] &)] := 0 /;
	Message[OrientedGraphPort::intport, vertexIndex, portIndex]

OrientedGraphPort[vertexIndex_ ? (IntegerQ[#] && # > 0 &), portIndex_ ? (NumericQ[#] && IntegerQ[#] && # <= 0 &)] := 0 /;
	Message[OrientedGraphPort::posport, vertexIndex, portIndex]

OrientedGraphPort[vertexIndex_ ?(IntegerQ[#] && # > 0 &), portIndex_ ? (IntegerQ[#] && # > 3 &)] := 0 /;
	Message[OrientedGraphPort::toolargeport, 1, vertexIndex, portIndex]


(* ::Subsection:: *)
(*Standard Form*)


$PortColor[OrientedGraphPort[vertexIndex_, slotIndex_]] := ColorData[97, slotIndex]


OrientedGraphPort /: MakeBoxes[OrientedGraphPort[vertexIndex_Integer, portIndex_ ? (MatchQ[1|2|3])], StandardForm] /;
	vertexIndex > 0 :=
	With[{portLabel = ToBoxes @ Style[vertexIndex, $PortColor @ OrientedGraphPort[vertexIndex, portIndex]]},
		InterpretationBox[portLabel, OrientedGraphPort[vertexIndex, portIndex]]
	]


(* ::Section:: *)
(*Outgoing Oriented Graph Port*)


(* ::Subsection:: *)
(*Consistency checks*)


OutgoingOrientedGraphPort::argx = "OrientedGraphPort called with `1` arguments; 1 argument are expected.";
OutgoingOrientedGraphPort::int = "Argument `1` in OrientedGraphPort[`1`] is not an integer.";
OutgoingOrientedGraphPort::pos = "Argument `1` in OrientedGraphPort[`1`] is not positive.";


OutgoingOrientedGraphPort[args___] := 0 /; Length @ {args} != 1 &&
	Message[OutgoingOrientedGraphPort::argx, Length @ {args}]

OutgoingOrientedGraphPort[vertexIndex_ ? (NumericQ[#] && !IntegerQ[#] &)] := 0 /;
	Message[OutgoingOrientedGraphPort::int, vertexIndex]

OutgoingOrientedGraphPort[vertexIndex_ ? (NumericQ[#] && IntegerQ[#] && # <= 0 &)] := 0 /;
	Message[OutgoingOrientedGraphPort::pos, vertexIndex]


(* ::Subsection:: *)
(*Standard Form*)


OutgoingOrientedGraphPort /: MakeBoxes[OutgoingOrientedGraphPort[vertexIndex_Integer], StandardForm] /;
	vertexIndex > 0 :=
	With[{portLabel = ToBoxes @ Slot @ vertexIndex},
		InterpretationBox[portLabel, OutgoingOrientedGraphPort[vertexIndex]]
	]


(* ::Section:: *)
(*Oriented Graph*)


(* ::Subsection:: *)
(*$PortQ*)


$PortQ[head_ ? (MatchQ[OrientedGraphPort | Subscript | List])[vertexIndex_Integer ? (# > 0 &), portIndex_ ? (MatchQ[1|2|3])]] := True

$PortQ[head_ ? (MatchQ[OutgoingOrientedGraphPort | Slot]) @ x_Integer ? (# > 0 &)] := True

$PortQ[arg___] := False


(* ::Subsection:: *)
(*$ToCanonicalEdge*)


$ToCanonicalPort[port_] := port /. {
	Subscript | List -> OrientedGraphPort,
	Slot -> OutgoingOrientedGraphPort
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


Attributes[OrientedGraphPort] = {ReadProtected};
Attributes[OrientedGraph] = {ReadProtected};
Attributes[OutgoingOrientedGraphPort] = {ReadProtected};


Protect[OrientedGraphPort, OrientedGraph, ReadProtected];


EndPackage[]
