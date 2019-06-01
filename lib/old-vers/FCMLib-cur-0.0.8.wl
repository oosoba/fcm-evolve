(* ::Package:: *)

(*Title: Fuzzy Cognitive Map Library - Representation + Evolution*)
(*Author: Osonde Osoba*)
(*Short Description:*)
(*This library implements routines for knowledge representation and causal simulation with*)
(*Fuzzy Cognitive Maps (FCMs). We use Mathematica's Graph class as the basic representation *)
(*for FCMs. And we use weighted adjacency matrices from Mathematica Graph objects as the secondary*)
(*representation and tool for simulating FCM dynamics.*)
(*See also: Python implementation (based on networkxx lib)*)
(*Timeline:*)
(*	- [Aug2016] Initial implementation written for JDMS special issue paper.*)
(*	- [Dec2017] Implementation updated for Social Simulation Book Chapter Write-up*)
(**)


BeginPackage["FCMLib`"]
$FCMLibVersion = "Fuzzy Cognitive Map Library ver. 0.0.8";
$activationBias = 0.5; (* Internal bias input. similar but not equal to threshold *)
$activationThreshold = 0.5; (* Threshold for judging node state *)
$activationFxn = UnitStep;
$comparator[t_]:=(UnitStep[t-$activationThreshold]);

SetAttributes[$comparator,{Listable,NumericFunction}];

(* Transitioning from number-indexed nodes to labeled nodes *)


(* ::Subsection:: *)
(*Exposed Fxns*)


FCM::usage = "Takes an FCM's weighted edge list matrix and returns a DAG object representing the FCM.";
FCMalt::usage = "Takes an FCM's weighted edge list matrix (text labeled) and returns a DAG object representing the FCM.";
FCMat::usage = "Returns weighted Adjacency matrix for a FCM specified by its weighted edge list or its DAG";

FCMJoin::usage = "Combines a list of FCMs using weighted addition. Only works if vertex labels are consistent";
FCMJoinByVote::usage = "Combines a list of FCMs using weighted addition only on majority-vote-ratified edges. Reqs: consistent vertex labels";

FCMpass::usage = "Returns a single-iteration response of the FCM to an input activation. Int in -> Int out, List in -> List out.";
FCMSeqIntCmp::usage = "Returns the FCM's evolution to its fixed point or limit cycle in response to an input";

FCMEvolSeq::usage = "FCM evoution to fp using string match index; Less fragile than integer conversions in FCMSeqIntCmp";

FCMAsyncpass::usage = "Returns a single step *Asynchronous* response of the FCM to an input activation. Int in -> Int out, List in -> List out.";
FCMAsyncRecallSeq::usage = "Returns the FCM's *Asynchronous* evolution to fixed point or limit cycle in response to an input";
FCMAsyncEvolSeq::usage = "Returns the FCM's *Asynchronous* evolution to fixed point or limit cycle in response to an input";

exoDrive::usage = "Returns vector with selected factor or factors activated. Node count req'd.";

FCMView::usage = "Visualize a FCM and its evolution over time (supplied in the second arg).";
FCMViewState::usage = "Visualize a mask state for specifiec FCM.";
annotatedMatrixPlot::usage = "Annotated Matrix Plotter for FCMs"

Clamp::usage = "clamper. Mostly for DIAGNOSTICS";
(*setActivation::usage = "change unit activation function and set important attrs.";*)

DHLHistory::usage = "Differential Hebbian Learning of concept-dyad causal edge weight. Returns learning history";
DHLFinalEstimate::usage = "return DHL estimated edge";
DHLMatrix::usage = "DHL for row-wise matrix of concept observations time-series.";

GHLHistory::usage = "Combined HL+DHL Learning Law for concept-dyad causal edge weight. Returns learning history";
GHLFinalEstimate::usage = "return GHL estimated edge";
GHLMatrix::usage = "HL+DHL for row-wise matrix of concept observation time-series.";

HebbHistory::usage = "Combined HL+DHL Learning Law for concept-dyad causal edge weight. Returns learning history";
HebbFinalEstimate::usage = "return GHL estimated edge";
HebbMatrix::usage = "HL+DHL for row-wise matrix of concept observation time-series.";

(* Meta parameters *)
$stdActvnParams::usage = "Gets the spec for std actvn (unitstep, 0.5)";


(* ::Subsection:: *)
(*Internals*)


Begin["`Private`"]

$stdActvnParams={UnitStep, 0.5}


(* ::Subsubsection:: *)
(*Representation*)


FCM[nodespec_?MatrixQ, edgespec_?MatrixQ, sz_Real:0.1, asz_Real:0.02]:=Graph[
Table[Tooltip[nd[[1]], nd[[1;;2]]], {nd,nodespec}], 
MapThread[DirectedEdge,Transpose@edgespec[[;;,;;2]]],
EdgeWeight->edgespec[[;;,-1]],
EdgeLabels->MapThread[Rule[DirectedEdge[#1,#2],#3]&,Transpose@edgespec[[;;,;;3]]],
EdgeShapeFunction->GraphElementData["FilledArrow","ArrowSize"->asz],
VertexSize->sz,
VertexLabels->Table[nd[[1]]->Placed[nd[[2]],Center],{nd, nodespec}]
(*VertexLabels->Table[i->Placed[nodespec[[i,2]],Center],{i,Length@Union@nodespec[[;;,2]] }]*)
]/;SubsetQ[
Union@nodespec[[;;,1]],
Union@Flatten[edgespec[[;;,;;2]]] 
]; (* use this as standard *)


FCM[spec_?MatrixQ, sz_Real:0.1, asz_Real:0.02]:=Graph[ 
Union@Flatten@spec[[;;,;;2]],
MapThread[DirectedEdge,Transpose@spec[[;;,;;2]]] ,
EdgeWeight->spec[[;;,-1]],
EdgeLabels->MapThread[Rule[DirectedEdge[#1,#2],#3]&,Transpose@spec[[;;,;;3]]],
EdgeShapeFunction->GraphElementData["FilledArrow","ArrowSize"->asz],
VertexSize->sz,
VertexLabels->Table[
	i->Placed["Name",Center],
	{i,Length@Union@Flatten@spec[[;;,{1,2}]] } 
	]
];


FCMalt[nodespec_?MatrixQ, edgespec_?MatrixQ, sz_Real:0.1, asz_Real:0.02]:=Graph[
Table[Tooltip[nd[[1]], nd], {nd,nodespec}], 
MapThread[DirectedEdge,Transpose@edgespec[[;;,;;2]]],
EdgeWeight->edgespec[[;;,-1]],
EdgeLabels->MapThread[Rule[DirectedEdge[#1,#2],#3]&,Transpose@edgespec[[;;,;;3]]],
EdgeShapeFunction->GraphElementData["FilledArrow","ArrowSize"->asz],
VertexSize->sz,
VertexLabels->Placed["Name",Center]
]/;SubsetQ[ 
Union@nodespec[[;;,1]],
Union@Flatten[edgespec[[;;,;;2]]] 
]; (* alternative node labelling *)


FCMat[spec_?MatrixQ]:=Normal@WeightedAdjacencyMatrix@FCM[spec];
FCMat[gr_?GraphQ]:=Normal@WeightedAdjacencyMatrix@gr;


(* ::Subsubsection::Closed:: *)
(*Combination*)


FCMJoin[nodespec_?MatrixQ, fcms_List,wgts_:0,sz_Real:0.25, asz_Real:0.02]:=Module[
	{n=Length@fcms, ws, fincm,newedgs},
	ws = If[
		n==Length@wgts,
		(wgts/Total[wgts]),
		ConstantArray[1/n,n]
	];
	newedgs=Join@@Last@Last@Reap@Do[
		Sow@Table[
			{e,ws[[k]]*PropertyValue[{fcms[[k]], e}, EdgeWeight]},
			{e, EdgeList@fcms[[k]]}
		],
		{k,Length@fcms}
	];
	newedgs=GroupBy[newedgs, First->Last, Total];
	newedgs=Normal@Select[newedgs,(#1!=0)&];

	fincm=Graph[
		Table[Tooltip[nd[[1]], nd[[2]]], {nd,nodespec}], 
		First/@newedgs,
		EdgeWeight->(Last/@newedgs),
		EdgeLabels->newedgs,
		EdgeShapeFunction->GraphElementData["FilledArrow","ArrowSize"->asz],
		VertexSize->sz,
		VertexLabels->Table[i->Placed[nodespec[[i,2]],Center],{i,Length@Union@nodespec[[;;,2]] }]
	];
	Return[fincm];
]


FCMJoinByVote[nodespec_?MatrixQ,fcms_List,wgts_:0,sz_Real:0.25, asz_Real:0.02]:=Module[
	{n=Length@fcms,ws,fincm,newedgs,ucands,candedgs=(EdgeList/@fcms),votes,vetoedLnks},
	ws = If[
		n==Length@wgts,
		(wgts/Total[wgts]),
		ConstantArray[1/n,n]
	];
	ucands = Sort@Union@Flatten@candedgs;
	votes=Outer[
		Boole@Not@FreeQ[candedgs[[#2]],#1]&,
		ucands, 
		Range@Length@candedgs
		]; (*candedgs is __List*)
	vetoedLnks = ucands[[Select[Range@Length@ucands, (Total[Transpose@votes][[#]]<Ceiling[n/2])&]]];
	fincm = Table[
		EdgeDelete[ f,Intersection[EdgeList[f], vetoedLnks] ], 
		{f,fcms}
	];
	fincm = FCMJoin[nodespec,fincm,ws];
	Return[fincm];
]


(* ::Subsubsection::Closed:: *)
(*Auxillary Fxns*)


S[x_]:= $activationFxn[x-$activationBias]*UnitStep[x]
(* Activations are notionally bivalent: either Binary (like here \[Element] {0,1}) or Bipolar \[Element]{-1,1} *)
(* threshold formerly hard-coded as 1/2 *)
(* Activation function formerly hard-coded as UnitStep[...] *)

(*S[x_]:=UnitStep[x-$unitThreshold]*)


(* Modified in v7+: allow clamping-on in (0,1] range *)
Clamp[actvn_List, mask_List]:=Module[
{
clmpd = actvn,
swon=Pick[Range@Length@actvn,mask, 1],
swleav=Pick[Range@Length@actvn,mask, x_/;x>0],
swoff=Pick[Range@Length@actvn,mask,x_/;x<0] (*selection crit should be x==-1*)
},
(* single mask forces disjoint on/off
clmpd now inherits and keeps actvns fuzzed 'on' values *)
clmpd[[swon]]=1;
clmpd[[swleav]]=clmpd[[swleav]];
clmpd[[swoff]]=0;
Return[clmpd];
]/;(Length[mask]==Length[actvn])

Clamp[actvn_List,on_Integer:0,off_Integer:0]:=Clamp[
actvn,
IntegerDigits[on,2,Length@actvn]-IntegerDigits[off,2,Length@actvn]
](* binary clamping *)


exoDrive[exo_Integer, len_Integer]:= Reverse@IntegerDigits[2^(exo-1), 2,len];
exoDrive[exos_List, len_Integer]:=Total[exoDrive[#,len]&/@exos];


(* ::Subsubsection::Closed:: *)
(*Evolution: Single Iteration*)


FCMpass[spec:(_?MatrixQ|_?GraphQ),inp_List, mask_List]:= Clamp[
S[
Clamp[inp,mask].FCMat[spec]
],
mask
]/;(Length[mask]==Length[inp]);




FCMpass[spec:(_?MatrixQ|_?GraphQ),inp_List,on_Integer:0,off_Integer:0]:= Clamp[
S[
Clamp[inp,on,off].FCMat[spec]
],
on,off
] ;


(* Returns integer representation *)
FCMpass[spec:(_?MatrixQ|_?GraphQ),inp_Integer,on_Integer:0,off_Integer:0]:=FromDigits[
FCMpass[
spec,
IntegerDigits[ inp,2, If[MatrixQ@spec, Length@Union@spec[[;;,1]],VertexCount@spec] ],
on,off
],
2
];

FCMpass[spec:(_?MatrixQ|_?GraphQ),inp_Integer,mask_List]:=FromDigits[
FCMpass[
spec,
IntegerDigits[ inp,2,If[MatrixQ@spec, Length@Union@spec[[;;,1]],VertexCount@spec] ],
mask
],
2
];



(* ::Subsubsection::Closed:: *)
(*Evolution to Termination*)


FCMEvolSeq[spec:(_?MatrixQ|_?GraphQ), inp_List, mask_List]:=NestWhileList[
	FCMpass[spec,#,mask]&, 
	inp,
	(Equal@@{
		Length@Union@Table[ToString[rc],{rc,{##}}],
		Length@Table[ToString[rc],{rc,{##}}]
	})&,
	All(*,12*)
];


(* FCMEvolSeq design stubs

inp=RandomInteger[{0,1}, n-1]~Join~{0};
mask=RandomInteger[{-1,1}, n-1]~Join~{0};
tst=NestWhileList[
FCMpass[psotFCMc,#,mask]&, 
inp,
(Print[{
Length@Union@Table[ToString[rc],{rc,{##}}],
Length@Table[ToString[rc],{rc,{##}}]
}];
Equal@@{
Length@Union@Table[ToString[rc],{rc,{##}}],
Length@Table[ToString[rc],{rc,{##}}]
})&,
All(*{1,\[Infinity]},,12*)
]//Grid*)


FCMSeqIntCmp[spec:(_?MatrixQ|_?GraphQ), inp_List, mask_List]:=(
IntegerDigits[
#,2,If[MatrixQ@spec, Length@Union@spec[[;;,1]],VertexCount@spec]
]&/@NestWhileList[
FCMpass[spec,#,mask]&, 
FromDigits[inp,2],
(Length@Union@Flatten@{##}==Length@Flatten@{##})&,
All,
\[Infinity] (*,1*)
])/;(Length[mask]==Length[inp]);


FCMSeqIntCmp[spec:(_?MatrixQ|_?GraphQ), inp_List, on_Integer:0, off_Integer:0]:=(IntegerDigits[
#,2,If[MatrixQ@spec, Length@Union@spec[[;;,1]],VertexCount@spec]
]&/@NestWhileList[
FCMpass[spec,#,on,off]&, 
FromDigits[inp,2],
(Length@Union@Flatten@{##}==Length@Flatten@{##})&,
All,
\[Infinity] (*,1*)
]);


(* ::Subsubsection::Closed:: *)
(*Visualization*)


FCMView[spec_?GraphQ,evol_?MatrixQ,labls_:0,sz_:1]:=Animate[
cur = $comparator@evol[[j]];
beglimcyc = First@FirstPosition[
#,First@Commonest[#]
]&@(FromDigits[#,2]&/@$comparator[evol]);

labs=Which[
	(VertexCount@spec==Length@labls), labls,
	True, Table[{i, Subscript["C",i]}, {i,VertexCount@spec}]
];

Graph[
spec,
VertexSize->sz,
VertexLabels->Table[nd[[1]]->Placed[Style[nd[[2]], 20], Center],{nd, labls}],
VertexStyle->MapThread[
Rule,
{
	Pick[VertexList@spec,cur,1], 
	Table[Lighter[If[j<beglimcyc,Red, Green], (1-evol[[j,k]])], 
		{k,Pick[Range@Length@labls,cur,1]}
	]
}
],
ImageSize->72*12,
Epilog->Inset[Framed[Style[Text[StringJoin@@(ToString/@{j," of ", Length@evol})],18],
	Background->LightYellow],{Left,Top},{Left,Top}]
],
{j,1,Length@evol,1},
AnimationRunning->False
]


FCMView[spec_?MatrixQ,evol_?MatrixQ,labls_:0,sz_:1]:=Animate[
cur = $comparator@evol[[j]];
beglimcyc = First@FirstPosition[
#,First@Commonest[#]
]&@(FromDigits[#,2]&/@$comparator[evol]);

labs=Which[
	(VertexCount@spec==Length@labls), labls,
	True,Table[{i,Subscript["C",i]},{i,VertexCount@spec}]
];

Graph[
Union[Flatten@spec[[;;,;;2]]], 
MapThread[DirectedEdge,Transpose@spec[[;;,;;2]]] ,
EdgeWeight->spec[[;;,-1]],
VertexSize->sz,
VertexLabels->Table[nd[[1]]->Placed[Style[nd[[2]], 20],Center],{nd, labls}],
VertexStyle->MapThread[
Rule,
{Pick[labls[[;;,1]],cur,1],ConstantArray[If[j<beglimcyc,Red, Green],Total@cur]}
],
ImageSize->72*12,
Epilog->Inset[Framed[Style[Text[StringJoin@@(ToString/@{j," of ", Length@evol})],18],
	Background->LightYellow],{Left,Top},{Left,Top}]
],
{j,1,Length@evol,1}]


FCMViewState[spec_?GraphQ,mask_List]:=Module[
{ons = Pick[VertexList@spec, mask, 1], offs = Pick[VertexList@spec, mask, -1]},
	Graph[
		spec,
		VertexStyle->{_?(MemberQ[offs,#]&)->Red, _?(MemberQ[ons,#]&)->Green},
		ImageSize->72*8
	]
]


annotatedMatrixPlot[mat_]:=Module[{
	ticks={
		{Table[{i,"C"<>ToString[i]},{i,1,Length@mat}],None},
		{None,Table[{i,"C"<>ToString[i]},{i,1,Length@mat}]}
		}
	},
	Return@MatrixPlot[
		mat,
		Epilog->{
			Black,
			MapIndexed[
			Style[Text[N[#1],Reverse[#2-1/2]], 18,Bold]&,
			Reverse[mat],{2}
		]},
		Mesh->True,FrameTicks->ticks
	]
]


(* ::Subsubsection::Closed:: *)
(*Asynchronous Evolution: Single Pass **)


FCMAsyncPass[inp_List, 
spec:(_?MatrixQ|_?GraphQ),subset_Integer:1,mask_List]:= Module[
{
actvn=inp,
pck = Sort@RandomSample[Range@Length@inp, Max[1,Min[subset,Length@inp]]]
},
(*Print[pck];*)
actvn[[pck]]=Clamp[
S[ Clamp[actvn,mask].(FCMat[spec][[;;,pck]]) ],
mask[[pck]]
];
Return[actvn];
]/;(Length[mask]==Length[inp]);


FCMAsyncPass[inp_List,spec:(_?MatrixQ|_?GraphQ),
subset_Integer:1,on_Integer:0,off_Integer:0]:= FCMAsyncPass[
inp,spec,subset,
IntegerDigits[on,2,Length@inp]-IntegerDigits[off,2,Length@inp]
]


(* Returns integer representation *)
FCMAsyncPass[inp_Integer,spec:(_?MatrixQ|_?GraphQ),subset_Integer:1,
mask_List]:=FromDigits[
FCMAsyncPass[
IntegerDigits[inp,2,Length@Union@spec[[;;,1]]
],
spec,subset,mask
],
2
];


FCMAsyncPass[inp_Integer,spec:(_?MatrixQ|_?GraphQ),subset_Integer:1,
on_Integer:0,off_Integer:0]:=FromDigits[
FCMAsyncPass[
IntegerDigits[inp,2,Length@Union@spec[[;;,1]]
],
spec,subset,on,off
],
2
];


(* ::Subsubsection::Closed:: *)
(*Asynchronous Evolution: Long-run Evol**)


(* ::Text:: *)
(*Limit Cycle Detection is flawed. Need to adapt current method to work of async/random evol*)


FCMAsyncEvolSeq[inp_List, spec:(_?MatrixQ|_?GraphQ),
	subset_Integer:1,mask_List]:=NestWhileList[
	FCMAsyncPass[#,spec,subset,mask]&, 
	inp,
	(Equal@@{
		Length@Union@Table[ToString[rc],{rc,{##}}],
		Length@Table[ToString[rc],{rc,{##}}]
	})&,
	All(*,12*)
];


FCMAsyncRecallSeq[inp_List, spec:(_?MatrixQ|_?GraphQ),subset_Integer:1,mask_List]:=(
IntegerDigits[
#,2,Length@Union@spec[[;;,1]]
]&/@NestWhileList[
FCMAsyncPass[#,spec,subset,mask]&, 
FromDigits[inp,2],(Length@Union@Flatten@{##}==Length@Flatten@{##})&,
All,
\[Infinity] (*,1*)
])/;(Length[mask]==Length[inp]);


FCMAsyncRecallSeq[inp_List, spec:(_?MatrixQ|_?GraphQ),subset_Integer:1,on_Integer:0,off_Integer:0]:=(
IntegerDigits[#,2,Length@Union@spec[[;;,1]]]&/@NestWhileList[
FCMAsyncPass[#,spec,subset,on,off]&, 
FromDigits[inp,2],
(Length@Union@Flatten@{##}==Length@Flatten@{##})&,
All,
\[Infinity] (*,1*)
]);


(* ::Subsubsection::Closed:: *)
(*Learning & Adaptation*)


(* ::Text:: *)
(*DHL*)


Options[DHLHistory]:={LearningRate-> ((0.1/Log[1+#])&)};

DHLHistory[C0_List, C1_List,opts___?OptionQ]:=Module[
	{\[Mu],ecur=0,enext=0,
	ns=Min@(Length/@{C0, C1}),
	\[CapitalDelta]C0=Differences@C0,\[CapitalDelta]C1=Differences@C1},
	{\[Mu]}={LearningRate}/.{opts}/.Options[DHLHistory];
	Return@Flatten@Last@Reap@Do[
		enext = ecur+ \[Mu][t]*Boole[\[CapitalDelta]C0[[t]]!=0]*(\[CapitalDelta]C0[[t]]*\[CapitalDelta]C1[[t]]-ecur);
		Sow@enext;
		ecur = enext;
		,{t,ns-1}
	]
];


DHLFinalEstimate[C0_List, C1_List,smooth_:25,opts___?OptionQ]:=Mean[
	DHLHistory[C0,C1, opts][[-smooth;;]]
];

DHLMatrixSubDyad[mat__List,ind1_Integer, ind2_Integer ]:=DHLFinalEstimate[
	mat[[ind1]],mat[[ind2]]
];

DHLMatrix[mat__List]:=Module[
	{fcm, cat=Subsets[Range@Length@mat, {2}], ind},(*Tuples*)
	fcm = ConstantArray[0, {Length@mat,Length@mat}];
	Do[
		fcm[[ind[[1]],ind[[2]]]] = DHLMatrixSubDyad[mat,ind[[1]],ind[[2]]],
		{ind,cat}
	];
	Return@fcm
];


(* ::Text:: *)
(*HL + DHL*)


Options[GHLHistory]:={
LearningRate->((0.1/#^0.95)&)
};
GHLHistory[C0_List, C1_List,opts___?OptionQ]:=Module[
{\[Mu],ecur=0,enext=0,
ns=Min@(Length/@{C0, C1}),
\[CapitalDelta]C0=Differences@C0,\[CapitalDelta]C1=Differences@C1
},
{\[Mu]}={LearningRate}/.{opts}/.Options[GHLHistory];
	Return@Flatten@Last@Reap@Do[
		enext = ecur+ 
			\[Mu][t]*(C0[[t+1]]*C1[[t+1]]-ecur) +
			\[Mu][t]*Boole[\[CapitalDelta]C0[[t]]!=0]*(\[CapitalDelta]C0[[t]]*\[CapitalDelta]C1[[t]]-ecur);
		Sow@enext;
		ecur = enext;
		,{t,ns-1}
	]
];


GHLFinalEstimate[C0_List, C1_List,smooth_:10,opts___?OptionQ]:=Mean[
	GHLHistory[C0,C1, opts][[-smooth;;]]
];
GHLMatrixSubDyad[mat__List,ind1_Integer, ind2_Integer ]:=GHLFinalEstimate[
	mat[[ind1]],mat[[ind2]]
];
GHLMatrix[mat__List]:=Module[(*assuming indices are in order of causal influence *)
	{fcm, cat=Subsets[Range@Length@mat, {2}], ind},(*Tuples*)
	fcm = ConstantArray[0, {Length@mat,Length@mat}];
	Do[
		fcm[[ind[[1]],ind[[2]]]] = GHLMatrixSubDyad[mat,ind[[1]],ind[[2]]],
		{ind,cat}
	];
	Return@fcm
];


(* ::Text:: *)
(*Hebb*)


Options[HebbHistory]:={LearningRate-> ((0.1/Log[1+#])&)};
HebbHistory[C0_List, C1_List,opts___?OptionQ]:=Module[
	{\[Mu],ecur=0,enext=0,ns=Min@(Length/@{C0, C1})},
	{\[Mu]}={LearningRate}/.{opts}/.Options[HebbHistory];
	Return@Flatten@Last@Reap@Do[
		enext = ecur+ \[Mu][t]*(C0[[t]]*C1[[t]]-ecur);
		Sow@enext;
		ecur = enext;
		,{t,ns}
	]
];

HebbFinalEstimate[C0_List, C1_List,smooth_:25,opts___?OptionQ]:=Mean[
	HebbHistory[C0,C1, opts][[-smooth;;]]
];
HebbMatrixSubDyad[mat__List,ind1_Integer, ind2_Integer ]:=HebbFinalEstimate[
	mat[[ind1]],mat[[ind2]]
];
HebbMatrix[mat__List]:=Module[(*assuming indices are in order of causal influence *)
	{fcm, cat=Subsets[Range@Length@mat, {2}], ind},(*Tuples*)
	fcm = ConstantArray[0, {Length@mat,Length@mat}];
	Do[
		fcm[[ind[[1]],ind[[2]]]] = HebbMatrixSubDyad[mat,ind[[1]],ind[[2]]],
		{ind,cat}
	];
	Return@fcm
];


(* ::Subsection::Closed:: *)
(*Outdated & Unexposed*)


(*(*FCMJoinOld[fcms_List, wgts_:0]:=Module[*)
(*{n=Length@fcms, ws, fincm},*)
(**)
(*ws = If[*)
(*And[(wgts!=0),n==Length@wgts],*)
(*wgts,*)
(*ConstantArray[1/n,n]*)
(*];*)
(*fincm=ws.(FCMat/@fcms);*)
(*fincm=ArrayRules[SparseArray@fincm][[;;-2]];*)
(*fincm=List@@@fincm;*)
(*fincm=Partition[Flatten[fincm],3];*)
(*Return[fincm];*)
(*]*)*)
(**)
(*(*FCMpass[inp_List, spec:(_?MatrixQ|_?GraphQ),on_List,off_List]:= Clamp[S[Clamp[inp,on-off].FCMat[spec]],on-off]/;(Length[on]==Length[inp]&&Length[off]==Length[inp]);*)*)
(*(* Use single mask list spec if using list spec *)*)


(*(* FCMpass[inp_List, spec:(_?MatrixQ|_?GraphQ)]:= S[inp.FCMat[spec]] *) (* Unclamped... *)*)


(* ::Subsection::Closed:: *)
(*Close Package*)


End[]


EndPackage[]
