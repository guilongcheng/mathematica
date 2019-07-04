(* ::Package:: *)

BeginPackage["GaussianInt`"]
Unprotect@@Names["GaussianInt`*"];
ClearAll@@Names["GaussianInt`*"];
GaussianInt::usage="GaussianInt[Intf_,vars_,Flag_:'Hide']:\:9ad8\:65af\:578b\:51fd\:6570\:7684\:79ef\:5206\:ff0cIntf\:88ab\:79ef\:51fd\:6570\:ff0cvars\:53d8\:91cf\:5217\:8868\:ff0c\:6ce8\:610f\:53d8\:91cf\:79ef\:5206\:8303\:56f4\:4e3a\:8d1f\:65e0\:7a77\:5230\:6b63\:65e0\:7a77\:3002Flag\:8bbe\:4e3aDebug\:65f6\:ff0c\:53ef\:4ee5\:6253\:5370\:4e2d\:95f4\:503c";
$GaussianDEBUG::usage="Deub flag";


Begin["`Private`"]


$GaussianDEBUG=False;
Nprecise=18;


(* ::Subsection:: *)
(*\:9ad8\:65af\:79ef\:5206\:90e8\:5206*)


GaussXx[newotherpart1_,Aeig_,M1_,ContExp1_,vars_]:=Block[{tmpresults,idexs,value,IndexCount,arrayrules,tmpCoeff},tmpresults=0;arrayrules=ArrayRules[newotherpart1];
Do[{idexs,value}={arrayrules[[i]][[1]],arrayrules[[i]][[2]]};IndexCount=Count[idexs,#]&/@Range[Length[vars]]
;If[And@@EvenQ/@IndexCount,(*Print[idexs,"    ",Aeig[[idexs[[1]]]],"  ",IndexCount,"; ",value];*)
(*tmpresults=tmpresults+value*Times@@Table[{1,1,3,15,105}[[IndexCount[[i]]/2+1]](1/Sqrt[2 Aeig[[i]]])^IndexCount[[i]],{i,Length[vars]}]],{i,Length[arrayrules]-1}];*)
tmpCoeff=Table[(2i-3)!!,{i,50}];
tmpresults=tmpresults+value*Times@@Table[tmpCoeff[[IndexCount[[i]]/2+1]](1/Sqrt[2 Aeig[[i]]])^IndexCount[[i]],{i,Length[vars]}]],{i,Length[arrayrules]-1}];
tmpresults=tmpresults*M1*ContExp1;tmpresults];

GaussianInt[Intff_,vars_]:=Block[{Intf,tmp,tmp1,exppart1,otherpart1,newotherpart1,ContExp1,DetA1,InvertA1,vcoeff1,rules,Acoeff,Aeig,Umatrix,NewVars,M1,IntAv1,sijkl1,fijk1,Dij1,alpha1,npars,results,tmpresults,IndexCount,idexs,value},
If[ListQ[Intff],Intf=Intff[[1]]//Simplify,Intf=Intff//Simplify];
npars=Length[vars];If[Intf==0,Return[0]];
tmp1=Collect[Intf,{E}];exppart1=Exponent[tmp1,{E}][[1]];(*\:83b7\:5f97\:6307\:6570\:9879*)
(*otherpart1=CoefficientArrays[tmp1/E^exppart1//Simplify,vars];*)
(*otherpart1=If[NumberQ[tmp1/.E^p_->1],tmp1/.E^p_->1,Simplify[tmp1/.E^p_->1]];*)
otherpart1=tmp1/.E^p_->1;(*\:83b7\:5f97\:5176\:5b83\:9879*)
(*\:6307\:6570\:9879\:90e8\:5206*)
ContExp1=Exp[Normal[CoefficientArrays[exppart1,vars][[1]]]];(*\:83b7\:5f97\:6307\:6570\:9879\:4e2d\:7684\:5e38\:6570\:9879*)
Acoeff=-Normal[CoefficientArrays[exppart1,vars][[3]]];(*\:83b7\:5f97\:6307\:6570\:9879\:4e2d\:7684\:4e8c\:6b21\:9879\:7cfb\:6570*)
Acoeff=(Transpose[Acoeff]+Acoeff)/2;(*\:5bf9\:79f0\:5316\:64cd\:4f5c*)
{Aeig,Umatrix}=Eigensystem[Acoeff];(*\:6c42\:89e3\:4e8c\:6b21\:9879\:7cfb\:6570\:7684\:672c\:5f81\:503c\:548c\:672c\:5f81\:77e2\:91cf*)
NewVars=Table[Symbol[ToString[vars[[i]]]<>"new"],{i,Length[vars]}];(*\:5b9a\:4e49\:65b0\:53d8\:91cf*)
DetA1=Det[Acoeff];(*\:4e8c\:6b21\:9879\:77e9\:9635\:7684\:884c\:5217\:5f0f*)
InvertA1=Inverse[Acoeff];(*\:4e8c\:6b21\:9879\:77e9\:9635\:7684\:9006*)
vcoeff1=Normal[CoefficientArrays[exppart1,vars][[2]]];(*\:4e00\:6b21\:9879\:7cfb\:6570*)
IntAv1=1/2 (InvertA1.vcoeff1);
rules=Table[vars[[i]]->( Transpose[Umatrix].NewVars+IntAv1)[[i]],{i,Length[vars]}];(*\:53d8\:91cf\:4ee3\:6362*)
newotherpart1=CoefficientArrays[otherpart1/.rules,NewVars];(*\:5176\:5b83\:90e8\:5206\:540c\:6837\:505a\:53d8\:91cf\:66ff\:6362*)
M1=Sqrt[\[Pi]^npars/DetA1] E^(1/4 vcoeff1.InvertA1.vcoeff1);(*\:5e38\:7cfb\:6570\:9879\:79ef\:5206*)
tmpresults=newotherpart1[[1]]*M1*ContExp1+If[Length[newotherpart1]<=2,0,Sum[GaussXx[newotherpart1[[i]],Aeig,M1,ContExp1,vars],{i,3,Length[newotherpart1],2}]];
If[$GaussianDEBUG,Print["\n Intf is ",Intf,"\n newotherpart1 is ",newotherpart1,"\n Length[newotherpart1[[3]]] is ",If[Length[newotherpart1]<=2,0,Length[newotherpart1[[3]]]],"\n otherpart1 is ",otherpart1,"\n ContExp is ", ContExp1, "\n Acoeff is ", Acoeff, "\n InvertA is ",InvertA1,"\n vcoeff is ",vcoeff1,"\n M is ",M1,"\n IntAv is ",IntAv1,"\n Umatrix is ",Umatrix,"\n Aeig is ",Aeig,"\n rules is ",rules,"\n newotherpart1 on vars is ",(tmp1/E^exppart1)/.rules//FullSimplify,"\n results is ",tmpresults]];
tmpresults];



End[]
EndPackage[]
