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


End[];


Attributes[OrientedGraphPort] = {ReadProtected};


Protect[OrientedGraphPort];


EndPackage[]
