(* Mathematica Package *)

(* Created by the Wolfram Workbench 23 Nov 2012 *)

BeginPackage["BugsBunny`"]
(* Exported symbols added here with SymbolName::usage *) 
BugsCH::usage="BugsCH[codaCHfile,codaIndexfile,obj] extracts an MCMC chain of obj from codaCHfile and codaindexfile."
BugsObjList::usage="BugsObjList[codaIndexfile] gives the list of the objects in codaIndexfile."

Begin["`Private`"]
(* Implementation of the package *)

BugsPos[codaIndexfile_,obj_]:=Module[{codaIndex,posIndx},

	If[FileExistsQ[codaIndexfile]==False,Print[codaIndexfile<>" is not found."];
		Abort[]];

	(* read index file *)
	codaIndex=Partition[ReadList[codaIndexfile,Word],3];
	(* find the position of the object *)
	posIndx=ToExpression@(codaIndex[[Position[codaIndex,obj]//Flatten//First]]//Rest);
	posIndx
];

BugsCH[codaCHfile_,codaIndexfile_,obj_]:=Module[{codaCH,codaobj,posindx},

	If[(FileExistsQ[codaIndexfile]||FileExistsQ[codaCHfile])==False,Print["File is not found."];
		Abort[]];

	(*read chain file*)
	codaCH=Partition[ReadList[codaCHfile,Word],2];
	
	(*posindx=ToExpression@(codaIndex[[Position[codaindx,obj]//Flatten//First]]//Rest);*)
	posindx = BugsPos[codaIndexfile,obj];
	(*codaobj=ToExpression@codaCH[[#1;;#2,2]]&@@posindx;*)
	codaobj=ToExpression@StringReplace[codaCH[[#1;;#2,2]]&@@posindx,"E"->"*^"];
	codaobj
];

BugsObjList[codaIndexfile_]:=Module[{codaIndex,objlist},

	If[FileExistsQ[codaIndexfile]==False,Print[codaIndexfile<>" is not found."];
		Abort[]];

	(* read index file *)
	codaIndex=Partition[ReadList[codaIndexfile,Word],3];
	objlist=codaIndex[[All,1]];
	objlist
];


End[]

EndPackage[]
