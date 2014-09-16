(* Wolfram Language package *)

(* Created by the Wolfram Workbench Sep 9, 2014 *)

BeginPackage["KurukuruW`CircularStats`", {"KurukuruW`"}]
(* Exported symbols added here with SymbolName::usage *) 

CircularWrap::usage = "Takes any phase value and returns the wrapped value in the range of (-Pi, Pi]."

CircularMinus::usage = "Phase subtraction returning value wrapped value in the range of (-Pi, Pi]."

CircularAccumulate::usage = "Accumulates phases such that each subsequent step is in the range (-Pi, Pi]."

Begin["`Private`"]
(* Implementation of the package *)

CircularWrap[phase_]:= 
Module[{temp},
	temp = Mod[phase, 2 * Pi];
	If[ temp <= - Pi, temp + 2 * Pi,
		If[ temp > Pi, temp - 2 * Pi,
			temp
		]
	]
];

SetAttributes[CircularWrap, Listable];

CircularWrap[args___]:=Message[CircularWrap::invalidArgs,{args}];

CircularMinus[phase1_, phase2_]:= CircularWrap[phase1 - phase2];

SetAttributes[CircularMinus, Listable];

CircularMinus[args___]:=Message[CircularMinus::invalidArgs,{args}];

CircularAccumulate[phases_List]:= Accumulate[ Prepend[ CircularMinus[#[[2]], #[[1]]]& /@ Partition[phases, 2, 1], phases[[1]] ] ];

CircularAccumulate[args___]:=Message[CircularAccumulate::invalidArgs,{args}];

End[]

EndPackage[]
