(* ::Package:: *)

BeginPackage["QNCModel`",{"GaussianInt`","HadronWaveF`"}]
Unprotect@@Names["QNCModel`*"];
ClearAll@@Names["QNCModel`*"];
GaussianInt::usage="GaussianInt[Intf_,vars_,Flag_:'Hide']:\:9ad8\:65af\:578b\:51fd\:6570\:7684\:79ef\:5206\:ff0cIntf\:88ab\:79ef\:51fd\:6570\:ff0cvars\:53d8\:91cf\:5217\:8868\:ff0c\:6ce8\:610f\:53d8\:91cf\:79ef\:5206\:8303\:56f4\:4e3a\:8d1f\:65e0\:7a77\:5230\:6b63\:65e0\:7a77\:3002Flag\:8bbe\:4e3aDebug\:65f6\:ff0c\:53ef\:4ee5\:6253\:5370\:4e2d\:95f4\:503c";
FlavorOverlap::usage="\:5473\:9053\:6ce2\:51fd\:6570\:8026\:5408 order\:4e3a\:672b\:6001\:6ce2\:51fd\:6570\:91cd\:6392\:987a\:5e8f x,y,z,v,w stand for ubar,dbar,sbar,cbar,bbar";
SpinOverlap::usage="\:81ea\:65cb\:6ce2\:51fd\:6570\:8026\:5408 order\:4e3a\:672b\:6001\:6ce2\:51fd\:6570\:91cd\:6392\:987a\:5e8f u \:4ee3\:8868\:81ea\:65cb\:5411\:4e0a\:ff0cd\:4ee3\:8868\:81ea\:65cb\:5411\:4e0b";
IntSpaceWavef::usage="\:7a7a\:95f4\:6ce2\:51fd\:6570\:8026\:5408 ";
Amplitude::usage="\:632f\:5e45\:ff0c";
AmplitudeTotal::usage="\:603b\:632f\:5e45";
QPCDecay::usage="\:8ba1\:7b97\:603b\:8870\:53d8";
MixQPCDecay::usage="MixQPCDecay[QNA,QNB,QNC,Amplitudes,MixCoeff],Amplitudes \:7ed9\:51fa\:9700\:8981\:6df7\:5408\:7684\:632f\:5e45\:ff0cMixingCoeff\:7ed9\:5b9a\:6df7\:5408\:7cfb\:6570";
MultiQPCDecay::usage="MultiQPCDecay[init,finals,Flag:'']";
MultiMixingQPCDecay::usage="MultiMixingQPCDecay[init,finals,Flag:'ShowResults']";
MixMultiDecays::usage="MixMultiDecays[init,finals]\:ff0c\:6ce8\:610ffinals\:7684\:683c\:5f0f";
ShowResults::usage="\:663e\:793a\:7ed3\:679c";
TranN6rep::usage="TranN6rep[QNpar],\:8f6c\:6362";
SpinFlavorOverlap::usage="\:81ea\:65cb\:5473\:9053\:8026\:5408";
$DEBUG::usage="\:6d4b\:8bd5\:ff0cTrue or False";
$DEBUGFlavor::usage="\:5473\:9053\:8026\:5408\:6d4b\:8bd5 True or False";
$DEBUGSpin::usage="\:81ea\:65cb\:8026\:5408\:6d4b\:8bd5 True or False";
$DEBUGSpinFlavor::usage="\:81ea\:65cb\:5473\:9053\:8026\:5408\:8c03\:8bd5\:5f00\:5173";
$DEBUGInt::usage="\:7a7a\:95f4\:79ef\:5206\:8c03\:8bd5\:5f00\:5173";
$DEBUGAmp::usage="\:632f\:5e45\:8c03\:8bd5\:5f00\:5173";
$DEBUGMultiDecay::usage="MultiDecay \:8c03\:8bd5\:5f00\:5173";
$DEBUGMixMultiDecays::usage="MixMultiDecays \:8c03\:8bd5";
$results::usage="\:7ed3\:679c";
$LorentzBoostFlag::usage="Lorentz Boost factor";
$IntegrateFlag::usage="\:79ef\:5206\:65b9\:6cd5\:ff0canalysis or numeric";
$QNCPars::usage="3P0\:8ba1\:7b97\:4e2d\:7684\:5404\:4e2a\:53c2\:6570";
$factor24::usage="24\:7ec4\:5408\:7684\:76f8\:540c\:56fe\:7684\:6570\:76ee";
$factor34::usage="34\:7ec4\:5408\:7684\:76f8\:540c\:56fe\:7684\:6570\:76ee";


Begin["`Private`"]


$DEBUG=False;
$DEBUGFlavor=False;
$DEBUGSpin=False;
$DEBUGSpinFlavor=False;
$DEBUGAmp=False;
$DEBUGInt=False;
$DEBUGMultiDecay=False;
$DEBUGMixMultiDecays=False;
$results={};
$LorentzBoostFlag=False;
$IntegrateFlag="analysis";
$QNCPars={};
$factor24=0;
$factor34=0;
QNCModel::kinematic="MA<MB+MC!";


(* ::Subsection:: *)
(*\:5473\:9053\:8026\:5408\:90e8\:5206*)


tranwaveform[doublelist_,srules_]:=doublelist[[;;,1]].StringReplace[doublelist[[;;,2]],srules];

JoinWavef[FlavorA_,FlavorB_]:=Block[{i,j,Flavorpart},
Flavorpart=<|"value"-> {}|>;
Flavorpart["symbol"]=FlavorA["symbol"]<>FlavorB["symbol"];
Do[Flavorpart["value"]=Append[Flavorpart["value"],{FlavorA["value"][[i]][[1]]*FlavorB["value"][[j]][[1]],FlavorA["value"][[i]][[2]]<>FlavorB["value"][[j]][[2]]}],{i,Length[FlavorA["value"]]},{j,Length[FlavorB["value"]]}];
Flavorpart];

FlavorOverlap[FlavorA_,FlavorOp_,FlavorB_,FlavorC_,order_]:=Block[{Flavorpart1,Flavorpart2,i,j,factor,tmp,transrules={"x"->"\!\(\*OverscriptBox[\(u\), \(-\)]\)","y"->"\!\(\*OverscriptBox[\(d\), \(-\)]\)","z"->"\!\(\*OverscriptBox[\(s\), \(-\)]\)","v"->"\!\(\*OverscriptBox[\(c\), \(-\)]\)","w"->"\!\(\*OverscriptBox[\(b\), \(-\)]\)"}},
If[Length[FlavorA["value"]]==0||Length[FlavorB["value"]]==0||Length[FlavorC["value"]]==0||Length[FlavorOp["value"]]==0,Return[<|"symbol"->0,"value"->0|>]];
Flavorpart1=JoinWavef[FlavorA,FlavorOp];
If[Length[Characters[Flavorpart1["value"][[1,2]]]]==4&&Characters[FlavorA["value"][[1,2]]][[1]]!= Characters[FlavorB["value"][[1,2]]][[1]],If[$DEBUGFlavor,Print["exchange B C!"]];Flavorpart2=JoinWavef[FlavorC,FlavorB],Flavorpart2=JoinWavef[FlavorB,FlavorC]];(*\:5473\:9053\:4ea4\:6362\:4e0d\:5f71\:54cd\:6700\:540e\:7ed3\:679c*)
tmp=Flavorpart2["value"];
Do[tmp[[i,2]]=StringJoin[Permute[Characters[Flavorpart2["value"][[i,2]]],Cycles[{order}]]],{i,Length[Flavorpart2["value"]]}];
Flavorpart2["value"]=tmp;
factor={};
Do[If[Flavorpart1["value"][[i]][[2]]==Flavorpart2["value"][[j]][[2]],factor=Append[factor,Flavorpart1["value"][[i]][[1]]*Flavorpart2["value"][[j]][[1]]]],{i,Length[Flavorpart1["value"]]},{j,Length[Flavorpart2["value"]]}];
tmp=Plus@@Flatten[factor];
If[$DEBUGFlavor==True,Print[{tranwaveform[Flavorpart1["value"],transrules],tranwaveform[Flavorpart2["value"],transrules],"FlavorOverlap"->tmp}]];<|"symbol"->"<"<>Flavorpart1["symbol"]<>"|"<>Flavorpart2["symbol"]<>">","value"-> tmp|>];


(* ::Subsection:: *)
(*\:81ea\:65cb\:8026\:5408\:90e8\:5206*)


SpinOverlap[SpinA_,SpinOp_,SpinB_,SpinC_,order_]:=Block[{chi3p0,Flavorpart1,Flavorpart2,i,j,factor,tmp},
If[Length[SpinA["value"]]==0||Length[SpinB["value"]]==0||Length[SpinC["value"]]==0||Length[SpinOp["value"]]==0,Return[<|"symbol"->0,"value"->0|>]];Flavorpart1=JoinWavef[SpinA,SpinOp];Flavorpart2=JoinWavef[SpinB,SpinC];
tmp=Flavorpart2["value"];
Do[tmp[[i,2]]=StringJoin[Permute[Characters[Flavorpart2["value"][[i,2]]],Cycles[{order}]]],{i,Length[Flavorpart2["value"]]}];
Flavorpart2["value"]=tmp;
factor={};
Do[If[Flavorpart1["value"][[i]][[2]]==Flavorpart2["value"][[j]][[2]],factor=Append[factor,Flavorpart1["value"][[i]][[1]]*Flavorpart2["value"][[j]][[1]]]],{i,Length[Flavorpart1["value"]]},{j,Length[Flavorpart2["value"]]}];
tmp=Plus@@Flatten[factor];
If[$DEBUGSpin==True,Print[{tranwaveform[Flavorpart1["value"],{"u"->"\[UpArrow]","d"->"\[DownArrow]"}],tranwaveform[Flavorpart2["value"],{"u"->"\[UpArrow]","d"->"\[DownArrow]"}],"SpinOverlap"->tmp}]];<|"symbol"-> "<"<>Flavorpart1["symbol"]<>"|"<>Flavorpart2["symbol"]<>">","value"-> tmp|>];


(* ::Subsection:: *)
(*\:81ea\:65cb\:5473\:9053\:8026\:5408\:90e8\:5206*)


SpinFlavorOverlap[SpinFlavorA_,SpinFlavorOp_,SpinFlavorB_,SpinFlavorC_,order_]:=Block[{LenA,LenB,LenC,tmp,tmp2,tmpf,tmps,tmpc},LenA=Length[SpinFlavorA];LenB=Length[SpinFlavorB];LenC=Length[SpinFlavorC];
If[LenA==0||LenB==0||LenB==0,Return[<|"symbol"->0,"value"->0|>]];
tmp={};tmp2=0;
Do[tmpf=FlavorOverlap[SpinFlavorA[[i,2]],SpinFlavorOp[[1,2]],SpinFlavorB[[j,2]],SpinFlavorC[[k,2]],order];
tmps=SpinOverlap[SpinFlavorA[[i,3]],SpinFlavorOp[[1,3]],SpinFlavorB[[j,3]],SpinFlavorC[[k,3]],order];
tmpc=SpinFlavorA[[i,1]]*SpinFlavorB[[j,1]]*SpinFlavorC[[k,1]];
If[tmpf["value"]==0||tmps["value"]==0||tmpc==0,0,tmp2=tmp2+ToString[InputForm[tmpc]]<>(tmps["symbol"]<>tmpf["symbol"]);tmp=Append[tmp,tmpc*tmps["value"]*tmpf["value"]];
If[$DEBUGSpinFlavor,Print["SpinFlavor: spinflavor coeff is ",tmpc,"; spin factor is ",tmps["value"],"; flavor factor is ",tmpf["value"]]];],
{i,LenA},{j,LenB},{k,LenC}];
<|"symbol"->tmp2,"value"-> Plus@@Flatten[tmp]|>];


(* ::Subsubsection:: *)
(*\:7a7a\:95f4\:79ef\:5206\:90e8\:5206*)


IntSpaceWavef[SpaceWavefA_,SpaceOp_,SpaceWavefB_,SpaceWavefC_]:=Block[{LenA,LenB,LenOp,LenC,tmpintf,tmpa,tmpb,tmpc,tmpop,pcut},{LenA,LenOp,LenB,LenC}=Length/@{SpaceWavefA["value"],SpaceOp["value"],SpaceWavefB["value"],SpaceWavefC["value"]};
Switch[$IntegrateFlag,
"analysis",tmpintf=Sum[GaussianInt[SpaceWavefA[["value",i]]*SpaceOp[["value",j]]*ComplexExpand[Conjugate[SpaceWavefB[["value",k]]]*Conjugate[SpaceWavefC[["value",l]]]],varp],{i,LenA},{j,LenOp},{k,LenB},{l,LenC}],
"numeric",pcut=Infinity;tmpa=Sum[SpaceWavefA["value"][[i]],{i,LenA}];tmpb=Sum[SpaceWavefB["value"][[i]],{i,LenB}];tmpc=Sum[SpaceWavefC["value"][[i]],{i,LenC}];tmpop=Sum[SpaceOp["value"][[i]],{i,LenOp}];
tmpintf=NIntegrate[tmpa*tmpb*tmpc*tmpop,{px,-pcut,pcut},{py,-pcut,pcut},{pz,-pcut,pcut}],
_,Print["IntegrateFlag error"];Return[0]];
If[$DEBUGInt,Print["IntSpace: GaussianInt factor is ",tmpintf,"\n Length SpaceWavef is ",{LenA,LenOp,LenB,LenC},"\n Intfunc is ",Table[SpaceWavefA[["value",i]]*SpaceOp[["value",1]]*ComplexExpand[Conjugate[SpaceWavefB[["value",j]]]*Conjugate[SpaceWavefC[["value",k]]]],{i,LenA},{j,LenOp},{k,LenB},{l,LenC}]]];
<|"symbol"-> "<"<>SpaceWavefA["symbol"]<>SpaceOp["symbol"]<>"|"<>SpaceWavefB["symbol"]<>SpaceWavefC["symbol"]<>">",
"value"->tmpintf|>];



(* ::Subsubsection:: *)
(*\:632f\:5e45\:53ca\:8870\:53d8\:5bbd\:5ea6*)


TranN6rep[QNpar_]:=Block[{N6,N3,MA,nA,JA,LA,SA,NameA,newName},If[NumberQ[QNpar["N6"]],{N6,N3,nA,JA,LA,SA,NameA}={QNpar["N6"],QNpar["N3"],QNpar["N"],QNpar["J"],QNpar["L"],QNpar["S"],QNpar["Name"]};newName=ToString[StringForm["|``\!\(\*SuperscriptBox[\(,\), \(``\)]\)``,``,``,\!\(\*SuperscriptBox[\(``\), \(``\)]\)>",N6,2*SA+1,N3,nA,LA,JA,If[(-1)^LA==1,"+","-"]],TraditionalForm],{nA,JA,LA,SA,NameA}={QNpar["N"],QNpar["J"],QNpar["L"],QNpar["S"],QNpar["Name"]};newName=ToString[StringForm["|\!\(\*SuperscriptBox[\(``\), \(``\)]\)\!\(\*SuperscriptBox[SubscriptBox[\(``\), \(``\)], \(``\)]\)>",nA+1,2*SA+1,Switch[LA,0,"S",1,"P",2,"D",3,"F",4,"G"],JA,If[(-1)^(LA+1)==1,"+","-"]],TraditionalForm]];newName];

Amplitude[psiA_,QNCop_,psiB_,psiC_,order_]:=Block[{LenA,LenB,LenC,tmpvalue,tmpsymbol,tmpsf,tmpspace,tmpfactors,m},LenA=Length[psiA];LenB=Length[psiB];LenC=Length[psiC];tmpvalue={};tmpsymbol=0;
Do[tmpsf=SpinFlavorOverlap[psiA[[i,2]],QNCop[[1,2]],psiB[[j,2]],psiC[[k,2]],order];
tmpfactors=psiA[[i,1]]*QNCop[[1,1]]*psiB[[j,1]]*psiC[[k,1]]*tmpsf["value"];
If[tmpfactors==0,0,tmpsymbol=tmpsymbol+psiA[[i,1]]*QNCop[[1,1]]*psiB[[j,1]]*psiC[[k,1]]*(tmpsf["symbol"]<>tmpspace["symbol"]);
tmpspace=IntSpaceWavef[psiA[[i,3]],QNCop[[1,3]],psiB[[j,3]],psiC[[k,3]]];tmpvalue=Append[tmpvalue,tmpfactors*tmpspace["value"]];
If[$DEBUGAmp,Print["Amp: coeff is ",{psiA[[i,1]],QNCop[[1,1]],psiB[[j,1]],psiC[[k,1]]},"; spinflavor factor is ",tmpsf["value"],"; space integrate factor is ",tmpspace["value"],"."];];],{i,LenA},{j,LenB},{k,LenC}];
<|"symbol"-> tmpsymbol,"value"-> Plus@@Flatten[tmpvalue]|>];

AmplitudeTotal[QNA_,QNB_,QNC_,pars_]:=Block[{\[Beta]A,\[Beta]B,\[Beta]C,\[Gamma],m1,m2,m3,m4,NameA,NameB,NameC,psiA,psiB,psiC,MA,nA,JA,LA,SA,MB,MC,EB,EC,nB,JB,LB,SB,nC,JC,LC,SC,AmpM,Amp34,Amp24,Amptotal,p\[Lambda],p\[Rho],pM,vecq,type,factor1,factor2,factor3,gammaf},
{\[Gamma],m1,m2,m3,m4}={pars["\[Gamma]"],pars["m1"],pars["m2"],pars["m3"],pars["m4"]};
{MA,nA,JA,LA,SA,NameA}={QNA["Mass"],QNA["N"],QNA["J"],QNA["L"],QNA["S"],QNA["Name"]};
{MB,nB,JB,LB,SB,NameB}={QNB["Mass"],QNB["N"],QNB["J"],QNB["L"],QNB["S"],QNB["Name"]};
{MC,nC,JC,LC,SC,NameC}={QNC["Mass"],QNC["N"],QNC["J"],QNC["L"],QNC["S"],QNC["Name"]};
If[MA<MB+MC,Message[QNCModel::kinematic];Return[0]];
vecq={0,0,Sqrt[(MA^2-(MB-MC)^2)(MA^2-(MB+MC)^2)]/(2MA)};EB=Sqrt[MB^2+vecq.vecq];
EC=Sqrt[MC^2+vecq.vecq];gammaf=If[$LorentzBoostFlag==True,MB/EB,1];vecq=vecq*gammaf;
Switch[{IntegerQ[JA],IntegerQ[JB]},
{True,True},pM={px,py,pz};varp=pM;Amptotal=Table[Sum[Amplitude[MesonWavef[QNA,MJA,MLA,MJA-MLA,pM-vecq],QNCop[MLB+MLC-MLA,pM],MesonWavef[QNB,MJB,MLB,MJB-MLB,-pM+m4/(m1+m4) vecq],MesonWavef[QNC,MJA-MJB,MLC,MJA-MJB-MLC,pM-(m4/(m2+m4))vecq],{2,4}],{MLA,Max[-LA,MJA-SA],Min[MJA+SA,LA]},{MLB,Max[-LB,MJB-SB],Min[LB,MJB+SB]},{MLC,Max[-LC,MJA-MJB-SC],Min[LC,MJA-MJB+SC]}],{MJA,-JA,JA},{MJB,-JB,JB}],
{False,False},p\[Rho]={p\[Rho]x,p\[Rho]y,p\[Rho]z};p\[Lambda]={p\[Lambda]x,p\[Lambda]y,p\[Lambda]z};varp=Join[p\[Rho],p\[Lambda]];factor1=3;factor2=0;
Amp34=Table[Sum[Amplitude[LBWavef[QNA,MJA,MLA,MJA-MLA,p\[Rho],p\[Lambda]],QNCop[MLB+MLC-MLA,vecq-Sqrt[2/3]p\[Lambda]],LBWavef[QNB,MJB,MLB,MJB-MLB,p\[Rho],p\[Lambda]-(Sqrt[6]m1)/(2m1+m4) vecq],MesonWavef[QNC,MJA-MJB,MLC,MJA-MJB-MLC,Sqrt[2/3]p\[Lambda]-m3/(m3+m4) vecq],{3,4}],{MLA,Max[-LA,MJA-SA],Min[MJA+SA,LA]},{MLB,Max[-LB,MJB-SB],Min[LB,MJB+SB]},{MLC,Max[-LC,MJA-MJB-SC],Min[LC,MJA-MJB+SC]}],{MJA,-JA,JA},{MJB,-JB,JB}];
If[m1!= m3&&m1== m4,Amp24=Table[Sum[Amplitude[LBWavef[QNA,MJA,MLA,MJA-MLA,p\[Rho],p\[Lambda]],QNCop[MLB+MLC-MLA,vecq+Sqrt[1/2]p\[Rho]+Sqrt[2/3] m2/(m1+m2) p\[Lambda]],LBWavef[QNB,MJB,MLB,MJB-MLB,(m1 (3 Sqrt[2] vecq+(2 Sqrt[3] (m2-m4) p\[Lambda])/(m1+m2)))/(3 (m1+m4))+p\[Rho],(Sqrt[3/2]*m3*vecq)/(m1+m3+m4)+p\[Lambda]],MesonWavef[QNC,MJA-MJB,MLC,MJA-MJB-MLC,-((m2*vecq)/(m2+m4))-(Sqrt[2/3] m2 p\[Lambda])/(m1+m2)-p\[Rho]/Sqrt[2]],{2,4}],{MLA,Max[-LA,MJA-SA],Min[MJA+SA,LA]},{MLB,Max[-LB,MJB-SB],Min[LB,MJB+SB]},{MLC,Max[-LC,MJA-MJB-SC],Min[LC,MJA-MJB+SC]}],{MJA,-JA,JA},{MJB,-JB,JB}],Amp24=0];
If[m1!= m3&&m3== m4,Amp24=Table[Sum[Amplitude[LBWavef[QNA,MJA,MLA,MJA-MLA,p\[Rho],p\[Lambda]],QNCop[MLB+MLC-MLA,\!\(TraditionalForm\`vecq + 
\*FractionBox[\(p\[Lambda]\), 
SqrtBox[\(6\)]] + 
\*FractionBox[\(p\[Rho]\), 
SqrtBox[\(2\)]]\)],LBWavef[QNB,MJB,MLB,MJB-MLB,(2 Sqrt[3] m4 p\[Lambda]+m3 (3 Sqrt[2] vecq+Sqrt[3] p\[Lambda]+3 p\[Rho]))/(3 (m3+m4)),\!\(TraditionalForm\`
\*FractionBox[\(
\*SqrtBox[\(3\)]\ \((
\*SqrtBox[\(2\)]\ m1\ vecq + \((m1 + m3 + m4)\)\ p\[Rho])\) - \((m1 + m3 + m4)\)\ p\[Lambda]\), \(2\ \((m1 + m3 + m4)\)\)]\)],MesonWavef[QNC,MJA-MJB,MLC,MJA-MJB-MLC,\!\(TraditionalForm\`\(-
\*FractionBox[\(m1\ vecq\), \(m1 + m4\)]\) - 
\*FractionBox[\(p\[Lambda]\), 
SqrtBox[\(6\)]] - 
\*FractionBox[\(p\[Rho]\), 
SqrtBox[\(2\)]]\)],{2,4}],{MLA,Max[-LA,MJA-SA],Min[MJA+SA,LA]},{MLB,Max[-LB,MJB-SB],Min[LB,MJB+SB]},{MLC,Max[-LC,MJA-MJB-SC],Min[LC,MJA-MJB+SC]}],{MJA,-JA,JA},{MJB,-JB,JB}],Amp24=0];
If[$factor24==0||$factor34==0,Print["Warning: $factor24 and $factor34 is zero!!!"]];
Amptotal=$factor34*Amp34+$factor24*Amp24,
_,0];If[$DEBUGAmp,Print["final momentum is ",Sqrt[vecq.vecq],";  Amplitude is :",Cases[Flatten[Amptotal],_?AssociationQ][[;;,"value"]],";  Amp34 is : ",Cases[Flatten[Amp34],_?AssociationQ][[;;,"value"]],";   Amp24 is : ",Cases[Flatten[Amp24],_?AssociationQ][[;;,"value"]]]];Amptotal
];

QPCDecay[QNA_,QNB_,QNC_,pars_]:=Block[{\[Beta]A,\[Beta]B,\[Beta]C,\[Gamma],m1,m2,m3,m4,NameA,NameB,NameC,psiA,psiB,psiC,MA,nA,JA,LA,SA,MB,MC,EB,EC,nB,JB,LB,SB,nC,JC,LC,SC,AmpM,Amp34,Amp24,Amptotal,p\[Lambda],p\[Rho],pM,vecq,type,factor1,factor2,factor3,gammaf,kinematicpart,decaywidth},
{\[Gamma],m1,m2,m3,m4}={pars["\[Gamma]"],pars["m1"],pars["m2"],pars["m3"],pars["m4"]};
{MA,nA,JA,LA,SA,NameA}={QNA["Mass"],QNA["N"],QNA["J"],QNA["L"],QNA["S"],QNA["Name"]};
{MB,nB,JB,LB,SB,NameB}={QNB["Mass"],QNB["N"],QNB["J"],QNB["L"],QNB["S"],QNB["Name"]};
{MC,nC,JC,LC,SC,NameC}={QNC["Mass"],QNC["N"],QNC["J"],QNC["L"],QNC["S"],QNC["Name"]};
If[MA<MB+MC,Message[QNCModel::kinematic];Return[0]];
vecq={0,0,Sqrt[(MA^2-(MB-MC)^2)(MA^2-(MB+MC)^2)]/(2MA)};EB=Sqrt[MB^2+vecq.vecq];
EC=Sqrt[MC^2+vecq.vecq];gammaf=If[$LorentzBoostFlag==True,MB/EB,1];vecq=vecq*gammaf;
Amptotal=AmplitudeTotal[QNA,QNB,QNC,pars];
kinematicpart=10^3*Pi^2 *gammaf*(vecq.vecq)^(1/2)/MA^2 1/(2JA+1) (8*MA*EB*EC)*\[Gamma]^2;
decaywidth=kinematicpart*Sum[Abs[Cases[Flatten[Amptotal],_?AssociationQ][[i]]["value"]]^2,{i,Length[Cases[Flatten[Amptotal],_?AssociationQ]]}];
$results=AppendTo[$results,{If[KeyExistsQ[pars,"process"],pars["process"],NameA<>TranN6rep[QNA]<>"->"<>NameB<>TranN6rep[QNB]<>NameC<>TranN6rep[QNC]],decaywidth,Cases[Flatten[Amptotal],_?AssociationQ][[;;,"value"]],kinematicpart,vecq[[3]]}];
];

MixQPCDecay[QNA_,QNB_,QNC_,pars_,Amplitudes_,MixingCoeff_]:=Block[{\[Beta]A,\[Beta]B,\[Beta]C,\[Gamma],m1,m2,m3,m4,NameA,NameB,NameC,psiA,psiB,psiC,MA,nA,JA,LA,SA,MB,MC,EB,EC,nB,JB,LB,SB,nC,JC,LC,SC,AmpM,Amp34,Amp24,Amptotal,p\[Lambda],p\[Rho],pM,vecq,type,factor1,factor2,factor3,gammaf,kinematicpart,decaywidth},
{\[Gamma],m1,m2,m3,m4}={pars["\[Gamma]"],pars["m1"],pars["m2"],pars["m3"],pars["m4"]};
{MA,nA,JA,LA,SA,NameA}={QNA["Mass"],QNA["N"],QNA["J"],QNA["L"],QNA["S"],QNA["Name"]};
{MB,nB,JB,LB,SB,NameB}={QNB["Mass"],QNB["N"],QNB["J"],QNB["L"],QNB["S"],QNB["Name"]};
{MC,nC,JC,LC,SC,NameC}={QNC["Mass"],QNC["N"],QNC["J"],QNC["L"],QNC["S"],QNC["Name"]};
If[MA<MB+MC,Message[QNCModel::kinematic];Return[0]];
vecq={0,0,Sqrt[(MA^2-(MB-MC)^2)(MA^2-(MB+MC)^2)]/(2MA)};EB=Sqrt[MB^2+vecq.vecq];
EC=Sqrt[MC^2+vecq.vecq];gammaf=If[$LorentzBoostFlag==True,MB/EB,1];vecq=vecq*gammaf;
Amptotal=Sum[Amplitudes[[i]]*MixingCoeff[[i]],{i,Length[MixingCoeff]}];
kinematicpart=10^3*Pi^2 *gammaf*(vecq.vecq)^(1/2)/MA^2 1/(2JA+1) (8*MA*EB*EC)*\[Gamma]^2;
decaywidth=kinematicpart*Sum[Abs[Cases[Flatten[Amptotal],_?AssociationQ][[i]]["value"]]^2,{i,Length[Cases[Flatten[Amptotal],_?AssociationQ]]}];
$results=AppendTo[$results,{If[KeyExistsQ[pars,"process"],pars["process"],NameA<>TranN6rep[QNA]<>"->"<>NameB<>TranN6rep[QNB]<>NameC<>TranN6rep[QNC]],decaywidth,Cases[Flatten[Amptotal],_?AssociationQ][[;;,"value"]],kinematicpart,vecq[[3]]}];
];

ShowResults[]:=Block[{total,Br,newresults},total=Sum[$results[[i,2]],{i,Length[$results]}];Br=Table[$results[[i,2]]/total*100,{i,Length[$results]}];Print["total Decay Width is ",total];newresults=Table[{$results[[i,1]],$results[[i,2]],Br[[i]],$results[[i,3]],$results[[i,4]],$results[[i,5]]},{i,Length[$results]}];newresults=Insert[newresults,{"Channel","Decay Width(MeV)","Br","Amplitude","Kinematic(MeV^2)","final momentum"},1];Print[newresults//MatrixForm];];

MultiQPCDecay[init_,final_,Flag_:""]:=Block[{QNA,QNB,QNC},
QNA=init;
If[$DEBUGMultiDecay,Print["QNA is ",QNA]];$results={};Do[QNB=final[[j,1]];QNC=final[[j,2]];
If[Length[final[[j]]]>=3,$QNCPars=Join[$QNCPars,final[[j,3]]]];
If[$DEBUGMultiDecay,Print["QNB,QNC,Pars are ",QNB,QNC,$QNCPars]];
QPCDecay[QNA,QNB,QNC,$QNCPars];,{j,Length[final]}];If[Flag=="ShowResults",ShowResults[]];];

MultiMixingQPCDecay[init_,final_,Flag_:"ShowResults"]:=Block[{QNA,QNB,QNC,Amps},Do[Amps={};QNA=init;
Do[
If[$DEBUG,Print[final[[j,jj,1]],"\n",final[[j,jj,2]],"\n",If[Length[final[[j,jj]]]>=3,final[[j,jj,3]]]]];
QNB=final[[j,jj,1]];QNC=final[[j,jj,2]];If[Length[final[[j,jj]]]>=3,$QNCPars=Join[$QNCPars,final[[j,jj,3]]]];
AppendTo[Amps,AmplitudeTotal[QNA,QNB,QNC,$QNCPars]];Print["Amps is ",Cases[Flatten[Amps[[jj-1]]],_?AssociationQ][[;;,"value"]]];,{jj,2,Length[final[[j]]]-1}];
If[$DEBUG,Print[QNB,QNC,Amps]];
QNB["Name"]=final[[j,1,1]];QNC["Name"]=final[[j,1,2]];
MixQPCDecay[QNA,QNB,QNC,$QNCPars,Amps,final[[j,-1]]],{j,Length[final]}];If[Flag=="ShowResults",ShowResults[]];];

MixMultiDecays[init_,finals_]:=Block[{QNA,QNB,QNC,Amps,coeff1,coeff2},$results={};
Do[If[$DEBUGMixMultiDecays,Print[{ListQ[init[[1]]],ListQ[finals[[j,1]]]}]];Switch[{ListQ[init[[1]]],ListQ[finals[[j,1]]]},
{True,True},
Amps={};Do[QNA=init[[ii]];
coeff1=init[[4,ii-1]];
QNB=finals[[j,jj,1]];QNC=finals[[j,jj,2]];
coeff2=finals[[j,-1,jj-1]];If[Length[finals[[j,jj]]]>=3,$QNCPars=Join[$QNCPars,finals[[j,jj,3]]]];AppendTo[Amps,coeff1*coeff2*AmplitudeTotal[QNA,QNB,QNC,$QNCPars]];,{jj,2,Length[finals[[j]]]-1},{ii,2,3}];
QNA["Name"]=init[[1]];
QNB["Name"]=finals[[j,1,1]];QNC["Name"]=finals[[j,1,2]];
If[$DEBUGMixMultiDecays,Print["MixCoeff is ",coeff1,coeff2,"\n Amplitudes are ",Amps]];
MixQPCDecay[QNA,QNB,QNC,$QNCPars,Amps,Table[1,{Length[init[[4]]]*Length[finals[[j,-1]]]}]];,
{False,True},
Amps={};Do[QNA=init;
QNB=finals[[j,jj,1]];QNC=finals[[j,jj,2]];
coeff2=finals[[j,-1,jj-1]];If[Length[finals[[j,jj]]]>=3,$QNCPars=Join[$QNCPars,finals[[j,jj,3]]]];AppendTo[Amps,coeff2*AmplitudeTotal[QNA,QNB,QNC,$QNCPars]];,{jj,2,Length[finals[[j]]]-1}];
QNB["Name"]=finals[[j,1,1]];QNC["Name"]=finals[[j,1,2]];
If[$DEBUGMixMultiDecays,Print["MixCoeff is ",coeff2,"\n Amplitudes are ",Amps]];
MixQPCDecay[QNA,QNB,QNC,$QNCPars,Amps,Table[1,{Length[finals[[j,-1]]]}]];,
{True,False},
Amps={};Do[QNA=init[[ii]];coeff1=init[[4,ii-1]];
QNB=finals[[j,1]];QNC=finals[[j,2]];
If[Length[finals[[j]]]>=3,$QNCPars=Join[$QNCPars,finals[[j,3]]]];AppendTo[Amps,coeff1*AmplitudeTotal[QNA,QNB,QNC,$QNCPars]];,{ii,2,3}];
QNA["Name"]=init[[1]];
MixQPCDecay[QNA,QNB,QNC,$QNCPars,Amps,Table[1,{Length[init[[4]]]}]];,
{False,False},
QNA=init;QNB=finals[[j,1]];QNC=finals[[j,2]];If[Length[finals[[j]]]>=3,$QNCPars=Join[$QNCPars,finals[[j,3]]]];QPCDecay[QNA,QNB,QNC,$QNCPars];,
_,0],{j,Length[finals]}];ShowResults[];]


End[]
EndPackage[]
