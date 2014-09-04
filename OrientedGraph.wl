(* ::Package:: *)

(* ::Title:: *)
(*Oriented Graph*)


(* ::Subtitle:: *)
(*Maxim Piskunov*)


BeginPackage["OrientedGraph`"];


Unprotect[OrientedGraphPort];


OrientedGraphPort::usage = StringJoin @ Riffle[{
	"OrientedGraphPort[1, v, i] yields the i-th port (that is an outlet to which edge connects) of the v-th vertex of an oriented graph",
	"OrientedGraphPort[2, i] yields the i-th outgoing port (that is an outlet which is free) of an oriented graph",
	"OrientedGraphPort[2, i, 1] is equivalent to OrientedGraphPort[2, i]"
}, "\n"];


Begin["OrientedGraph`Private`"];


(* ::Section:: *)
(*Oriented Graph Port*)


(* ::Subsection:: *)
(*Consistency checks*)


OrientedGraphPort::invtp = "First argument `1` of OrientedGraphPort is neither 1 nor 2.";
OrientedGraphPort::argx = "OrientedGraphPort called with `1` arguments; 2 or 3 arguments are expected.";
OrientedGraphPort::inargx = "OrientedGraphPort called with first argument 1 and `1` arguments total; 3 arguments are expected in that case.";
OrientedGraphPort::intvert = "Second argument `1` in OrientedGraphPort is not an integer.";
OrientedGraphPort::posvert = "Second argument `1` in OrientedGraphPort is not positive.";
OrientedGraphPort::intport = "Third argument in OrientedGraphPort[`1`, `2`, `3`] is not an integer.";
OrientedGraphPort::posport = "Third argument in OrientedGraphPort[`1`, `2`, `3`] is not positive.";
OrientedGraphPort::toolargeport = "Only oriented graphs with 3 ports per vertex are supported.";
OrientedGraphPort::intoutport = "Third argument in outgoing port OrientedGraphPort[`1`, `2`, `3`] is not 1.";


OrientedGraphPort[2, vertexIndex_ ? (IntegerQ[#] && # > 0 &)] := OrientedGraphPort[2, vertexIndex, 1]


OrientedGraphPort[type_ ? (NumericQ[#] && # != 1 && # != 2 &), vertexIndex_, portIndex_] := 0 /;
	Message[OrientedGraphPort::invtp, type]

OrientedGraphPort[type_ ? (NumericQ[#] && # != 1 && # != 2 &), vertexIndex_] := 0 /;
	Message[OrientedGraphPort::invtp, type]

OrientedGraphPort[args___] := 0 /; !MatchQ[Length @ {args}, 2|3] &&
	Message[OrientedGraphPort::argx, Length @ {args}]

OrientedGraphPort[1, vertexIndex_] := 0 /;
	Message[OrientedGraphPort::inargx, 2]

OrientedGraphPort[2, vertexIndex_ ? (NumericQ[#] && !IntegerQ[#] &)] := 0 /;
	Message[OrientedGraphPort::intvert, vertexIndex]

OrientedGraphPort[2, vertexIndex_ ? (NumericQ[#] && IntegerQ[#] && # <= 0 &)] := 0 /;
	Message[OrientedGraphPort::posvert, vertexIndex]

OrientedGraphPort[type_ ? (MatchQ[1|2]), vertexIndex_ ? (NumericQ[#] && !IntegerQ[#] &), portIndex_] := 0 /;
	Message[OrientedGraphPort::intvert, vertexIndex]

OrientedGraphPort[type_ ? (MatchQ[1|2]), vertexIndex_ ? (NumericQ[#] && IntegerQ[#] && # <= 0 &), portIndex_] := 0 /;
	Message[OrientedGraphPort::posvert, vertexIndex]

OrientedGraphPort[type_ ? (MatchQ[1|2]), vertexIndex_ ? (IntegerQ[#] && # > 0 &), portIndex_ ? (NumericQ[#] && !IntegerQ[#] &)] := 0 /;
	Message[OrientedGraphPort::intport, type, vertexIndex, portIndex]

OrientedGraphPort[type_ ? (MatchQ[1|2]), vertexIndex_ ? (IntegerQ[#] && # > 0 &), portIndex_ ? (NumericQ[#] && IntegerQ[#] && # <= 0 &)] := 0 /;
	Message[OrientedGraphPort::posport, type, vertexIndex, portIndex]

OrientedGraphPort[1, vertexIndex_ ?(IntegerQ[#] && # > 0 &), portIndex_ ? (IntegerQ[#] && # > 3 &)] := 0 /;
	Message[OrientedGraphPort::toolargeport, 1, vertexIndex, portIndex]

OrientedGraphPort[2, vertexIndex_ ?(IntegerQ[#] && # > 0 &), portIndex_ ? (IntegerQ[#] && # > 1 &)] := 0 /;
	Message[OrientedGraphPort::intoutport, 1, vertexIndex, portIndex]


End[];


Attributes[OrientedGraphPort] = {ReadProtected};


Protect[OrientedGraphPort];


EndPackage[]
