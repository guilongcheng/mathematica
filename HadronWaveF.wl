(* ::Package:: *)

BeginPackage["HadronWaveF`"]
Unprotect@@Names["HadronWaveF`*"];
ClearAll@@Names["HadronWaveF`*"];

psiMCart::usage="psiMCart[\[Beta]_,n_,L_,M_,p_]:\:8c10\:632f\:5b50\:52bf\:4e0b\:4ecb\:5b50\:76f4\:89d2\:5750\:6807\:7cfb\:4e0b\:7a7a\:95f4\:6ce2\:51fd\:6570,\:6ce8\:610fp_\:4e3a\:4e00\:4e2a\:52a8\:91cf\:5217\:8868\:4f8b\:5982{px,py,pz}";
psiBCart::usage="psiBCart[N_,L_,LM_,S3_,\[Beta]\[Rho]_,\[Beta]\[Lambda]_,p\[Rho]_,p\[Lambda]_]:\:8c10\:632f\:5b50\:52bf\:4e0b\:91cd\:5b50\:76f4\:89d2\:5750\:6807\:7cfb\:4e0b\:7a7a\:95f4\:6ce2\:51fd\:6570";
ShpCart::usage="ShpCart[L_,M_,p_]: 3p0\:4e2d\:7684\:7a7a\:95f4\:7b97\:7b26\:3002\:5b9a\:4e49\:4e3aP x |1,m>";
RshoCart::usage="RshoCart[\[Beta],n,L,{px,py,pz}]:\:5f84\:5411\:6ce2\:51fd\:6570\:7684\:6b63\:5219\:5750\:6807\:8868\:793a";
LBFlavorWavef::usage="LBFlavorWavef[S3,Name]\:8f7b\:91cd\:5b50\:5473\:9053\:6ce2\:51fd\:6570";
LBSpinWavef::usage="LBSpinWavef[S3,S,MS]\:8f7b\:91cd\:5b50\:81ea\:65cb\:6ce2\:51fd\:6570";
LBSpinFlavorWavef::usage="LBSpinFlavorWavef[QNpar_,MS_,S3_]:\:8f7b\:91cd\:5b50\:81ea\:65cb\:5473\:9053\:6ce2\:51fd\:6570";
LBSpaceWavef::usage="LBSpaceWavef[QNpar_,ML_,S3_,p__]\:8f7b\:91cd\:5b50\:7a7a\:95f4\:6ce2\:51fd\:6570";
LBWavef::usage="LBWavef[QNpar_,MJ_,ML_,MS_,p__]:\:8f7b\:91cd\:5b50\:6ce2\:51fd\:6570";
MesonSpinWavef::usage="MesonSpinWavef[S_,MS_]\:4ecb\:5b50\:81ea\:65cb\:6ce2\:51fd\:6570";
MesonFlavorWavef::usage="MesonFlavorWavef[Name_],\:4ecb\:5b50\:5473\:9053\:6ce2\:51fd\:6570";
MesonSpinFlavorWavef::usage="MesonSpinFlavorWavef[QNpar_,MS_]:\:4ecb\:5b50\:81ea\:65cb\:5473\:9053\:6ce2\:51fd\:6570";
MesonSpaceWavef::usage="MesonSpaceWavef[QNpar_,ML_,p_]:\:4ecb\:5b50\:7a7a\:95f4\:6ce2\:51fd\:6570";
MesonWavef::usage="MesonWavef[QNpar_,MJ_,ML_,MS_,p__]:\:4ecb\:5b50\:6ce2\:51fd\:6570";
FitMGECoeff::usage="FitMGECoeff[data_,QNpar_,Flag_:'Normal']:\:62df\:5408\:6570\:503c\:6570\:636e\:7ed9\:51fa\:591a\:9ad8\:65af\:5c55\:5f00\:7cfb\:6570\:ff0cdata={{r1,f1},...},Flag\:53ef\:4ee5\:9009\:62e9Debug\:ff0cShow\:ff0c\:9ed8\:8ba4\:4e3aNormal";
GetMGECoeff::usage="GetMGECoeff[path,QNpar]:\:83b7\:53d6\:591a\:9ad8\:65af\:5c55\:5f00\:7cfb\:6570";
MGEr::usage="MGEr[QNpar_,r_]:\:591a\:9ad8\:65af\:5c55\:5f00\:5750\:6807\:7a7a\:95f4\:57fa\:51fd\:6570";
MGEp::usage="MGEr[QNpar_,p_]:\:591a\:9ad8\:65af\:5c55\:5f00\:52a8\:91cf\:7a7a\:95f4\:57fa\:51fd\:6570";
HarmonicSpacep::usage="HarmonicSpacep[QNpar_,p_]:\:8c10\:632f\:5b50\:52a8\:91cf\:7a7a\:95f4\:7a7a\:95f4\:6ce2\:51fd\:6570";
HarmonicSpacer::usage="HarmonicSpacer[QNpar_,r_]:\:8c10\:632f\:5b50\:5750\:6807\:7a7a\:95f4\:7a7a\:95f4\:6ce2\:51fd\:6570";
QNCop::usage="QNCop[m_,p__]:3p0\:7b97\:7b26";
$fmtoGeV::usage="\:5982\:679c\:6570\:503c\:6ce2\:51fd\:6570\:7684r\:4ee5fm\:4e3a\:5355\:4f4d\:ff0c\:5219\:8bbe\:7f6e\:4e3aTrue";


Begin["`Private`"]
$fmtoGeV=True;
HadronWaveF::flavor="can't find this particle in flavor list";


(* ::Subsubsection:: *)
(*\:7a7a\:95f4\:6ce2\:51fd\:6570*)


(*\:5f84\:5411\:76f4\:89d2\:5750\:6807\:7cfb\:6ce2\:51fd\:6570,\:6ce8\:610fRshoCart\:4e2d\:7684p^L\:653e\:5165\:4e86ShpCart\:51fd\:6570\:4e2d*)
RshoCart[\[Beta]_,n_,L_,p_]:=LaguerreL[n,L+1/2,(p[[1]]^2+p[[2]]^2+p[[3]]^2)/\[Beta]^2] ((-1)^n*(-I)^L)/\[Beta]^(3/2) Sqrt[(2n!)/Gamma[n+L+3/2]] (1/\[Beta])^L E^(-(p[[1]]^2+p[[2]]^2+p[[3]]^2)/(2 \[Beta]^2));

Am[M_,p_]:=Sum[Binomial[M,k]p[[1]]^k p[[2]]^(M-k) Cos[(M-k)\[Pi]/2],{k,0,M}];

Bm[M_,p_]:=Sum[Binomial[M,k]p[[1]]^k p[[2]]^(M-k) Sin[(M-k)\[Pi]/2],{k,0,M}];

ALegPolyCart[L_,M_,p_]:=If[M==0,Sum[(-1)^k 2^-L Binomial[L,k]Binomial[2L-2k,L](p[[1]]^2+p[[2]]^2+p[[3]]^2)^k p[[3]]^(L-2k),{k,0,IntegerPart[L/2]}],Sqrt[(L-M)!/(L+M)!]Sum[(-1)^k 2^-L Binomial[L,k]Binomial[2L-2k,L] (L-2k)!/(L-2k-M)! (p[[1]]^2+p[[2]]^2+p[[3]]^2)^k p[[3]]^(L-2k-M),{k,0,IntegerPart[(L-M)/2]}]];

ShpCart[L_,M_,p_]:=Block[{},If[Abs[M]>L,Return[0]];If[M==0,Sqrt[(2L+1)/(4\[Pi])]ALegPolyCart[L,0,p],If[M>0,Sqrt[(2L+1)/(4\[Pi])]ALegPolyCart[L,M,p]((-1)^M (Am[M,p]+I Bm[M,p])),Sqrt[(2L+1)/(4\[Pi])]ALegPolyCart[L,-M,p]((Am[-M,p]-I Bm[-M,p]))]]];

psiMCart[\[Beta]_,n_,L_,M_,p_]:=If[Abs[M]>L,0,RshoCart[\[Beta],n,L,p]*ShpCart[L,M,p]];
(*psiBCart[\[Beta]\[Rho]_,n\[Rho]_,L\[Rho]_,m\[Rho]_,p\[Rho]_,\[Beta]\[Lambda]_,n\[Lambda]_,L\[Lambda]_,m\[Lambda]_,p\[Lambda]_]:=psiMCart[\[Beta]\[Rho],n\[Rho],L\[Rho],m\[Rho],p\[Rho]]*psiMCart[\[Beta]\[Lambda],n\[Lambda],L\[Lambda],m\[Lambda],p\[Lambda]]*)

psiBCart[n_,L_,LM_,S3_,\[Beta]\[Rho]_,\[Beta]\[Lambda]_,p\[Rho]_,p\[Lambda]_]:=If[LM>Abs[L],Return[0],Switch[{n,L,S3},
{0,0,"S"},psiMCart[\[Beta]\[Rho],0,0,0,p\[Rho]]*psiMCart[\[Beta]\[Lambda],0,0,0,p\[Lambda]],
{1,1,"\[Rho]"},psiMCart[\[Beta]\[Rho],0,L,LM,p\[Rho]]*psiMCart[\[Beta]\[Lambda],0,0,0,p\[Lambda]],
{1,1,"\[Lambda]"},psiMCart[\[Beta]\[Rho],0,0,0,p\[Rho]]*psiMCart[\[Beta]\[Lambda],0,L,LM,p\[Lambda]],
{2,0,"S"},1/Sqrt[2] (psiMCart[\[Beta]\[Rho],1,0,0,p\[Rho]]*psiMCart[\[Beta]\[Lambda],0,0,0,p\[Lambda]]+psiMCart[\[Beta]\[Rho],0,0,0,p\[Rho]]*psiMCart[\[Beta]\[Lambda],1,0,0,p\[Lambda]]),
{2,0,"\[Lambda]"},1/Sqrt[2] (psiMCart[\[Beta]\[Rho],1,0,0,p\[Rho]]*psiMCart[\[Beta]\[Lambda],0,0,0,p\[Lambda]]-psiMCart[\[Beta]\[Rho],0,0,0,p\[Rho]]*psiMCart[\[Beta]\[Lambda],1,0,0,p\[Lambda]]),
{2,0,"\[Rho]"},Sum[ClebschGordan[{1,lm},{1,-lm},{0,0}]*psiMCart[\[Beta]\[Rho],0,1,lm,p\[Rho]]*psiMCart[\[Beta]\[Lambda],0,1,-lm,p\[Lambda]],{lm,-1,1}],
{2,1,"a"},(ClebschGordan[{1,LM-Abs[LM]+1},{1,-1+Abs[LM]},{1,LM}]*psiMCart[\[Beta]\[Rho],0,1,LM-Abs[LM]+1,p\[Rho]]*psiMCart[\[Beta]\[Lambda],0,1,-1+Abs[LM],p\[Lambda]]-ClebschGordan[{1,-1+Abs[LM]},{1,LM-Abs[LM]+1},{1,LM}]*psiMCart[\[Beta]\[Rho],0,1,-1+Abs[LM],p\[Rho]]*psiMCart[\[Beta]\[Lambda],0,1,LM-Abs[LM]+1,p\[Lambda]]),
{2,2,"S"},1/Sqrt[2] (psiMCart[\[Beta]\[Rho],0,2,LM,p\[Rho]]*psiMCart[\[Beta]\[Lambda],0,0,0,p\[Lambda]]+psiMCart[\[Beta]\[Rho],0,0,0,p\[Rho]]*psiMCart[\[Beta]\[Lambda],0,2,LM,p\[Lambda]]),
{2,2,"\[Lambda]"},1/Sqrt[2] (psiMCart[\[Beta]\[Rho],0,2,LM,p\[Rho]]*psiMCart[\[Beta]\[Lambda],0,0,0,p\[Lambda]]-psiMCart[\[Beta]\[Rho],0,0,0,p\[Rho]]*psiMCart[\[Beta]\[Lambda],0,2,LM,p\[Lambda]]),
{2,2,"\[Rho]"},(ClebschGordan[{1,If[LM<0,-1,1]},{1,LM-If[LM<0,-1,1]},{2,LM}]*psiMCart[\[Beta]\[Rho],0,1,If[LM<0,-1,1],p\[Rho]]*psiMCart[\[Beta]\[Lambda],0,1,LM-If[LM<0,-1,1],p\[Lambda]]+If[Abs[LM]<2,1,0]ClebschGordan[{1,LM-If[LM<0,-1,1]},{1,If[LM<0,-1,1]},{2,LM}]*psiMCart[\[Beta]\[Rho],0,1,LM-If[LM<0,-1,1],p\[Rho]]*psiMCart[\[Beta]\[Lambda],0,1,If[LM<0,-1,1],p\[Lambda]]+
If[LM==0,1,0]ClebschGordan[{1,0},{1,0},{2,0}]*psiMCart[\[Beta]\[Rho],0,1,0,p\[Rho]]*psiMCart[\[Beta]\[Lambda],0,1,0,p\[Lambda]]),
_,0]];

HarmonicSpacep[QNpar_,p_]:=Block[{n,L,\[Beta]},{n,L,\[Beta]}={QNpar["N"],QNpar["L"],QNpar["\[Beta]"]};LaguerreL[n,L+1/2,p^2/\[Beta]^2] ((-1)^n*(-I)^L)/\[Beta]^(3/2) Sqrt[(2n!)/Gamma[n+L+3/2]] (p/\[Beta])^L E^(-(p^2) /(2 \[Beta]^2))];
HarmonicSpacer[QNpar_,r_]:=Block[{n,L,\[Beta]},{n,L,\[Beta]}={QNpar["N"],QNpar["L"],QNpar["\[Beta]"]};\[Beta]^(3/2)*(((2^(L+2-n)) (2L+2n+1)!! )/(Sqrt[\[Pi]]n! ((2L+1)!!)^2))^(1/2)*(\[Beta] r)^L*E^((-\[Beta]^2 r^2)/2) *  Hypergeometric1F1 [-n,L+3/2,\[Beta]^2 r^2]];

MGEp[QNpar_,p_]:=Block[{L,a0,q,Nmax},{L,a0,q,Nmax}={QNpar["L"],QNpar["a0"],QNpar["q"],QNpar["Nmax"]};
If[$fmtoGeV,a0=a0/0.1973];
If[Length[p]>1,
Table[((-1)^L*(2/Pi)^(1/4)*(a0*q^((k - 1)/(Nmax-1)))^(3/2 + L)*Sqrt[1/(1 + 2*L)!!])/E^(((p[[1]]^2+p[[2]]^2+p[[3]]^2)*(a0*q^((k - 1)/(Nmax-1)))^2)/4), {k, Nmax}],
Table[((-1)^L*(2/Pi)^(1/4)*(a0*q^((k - 1)/(Nmax-1)))^(3/2 + L)*Sqrt[1/(1 + 2*L)!!])/E^(((p^2)*(a0*q^((k - 1)/(Nmax-1)))^2)/4), {k, Nmax}]]];

MGEr[QNpar_,r_]:=Block[{L,a0,q,Nmax,vn,Nnl},{L,a0,q,Nmax}={QNpar["L"],QNpar["a0"],QNpar["q"],QNpar["Nmax"]};
vn=Table[1/(a0*(q)^((i-1)/(Nmax-1)))^2,{i,1,Nmax}];
Nnl=Table[((2^(L+2)*(2*vn[[i]])^(L+3/2))/(Sqrt[\[Pi]](2*L+1)!!))^(1/2),{i,1,Nmax}];
If[Length[r]>1,Table[Nnl[[i]]*Sqrt[r[[1]]^2+r[[2]]^2+r[[3]]^2]^L*E^(-vn[[i]]),{i,1,Nmax}],
Table[Nnl[[i]]*E^(-vn[[i]]*r^2),{i,1,Nmax}]]];

FitMGECoeff[data_,QNpar_,Flag_:"Normal"]:=Block[{vn,Eij,Nnl,\[Phi]r,wavef,norm,FOverlapGBF,coeffGBF,SumGaussian,L,a0,q,Nmax,h},
h=data[[2,1]]-data[[1,1]];
{L,a0,q,Nmax}={QNpar["L"],QNpar["a0"],QNpar["q"],QNpar["Nmax"]};
vn=Table[1/(a0*(q)^((i-1)/(Nmax-1)))^2,{i,1,Nmax}];
Eij=Table[N[((2Sqrt[vn[[i]]vn[[j]]])/(vn[[i]]+vn[[j]]))^(L+3/2)],{i,1,Nmax},{j,1,Nmax}];
Nnl=Table[((2^(L+2)*(2*vn[[i]])^(L+3/2))/(Sqrt[\[Pi]](2*L+1)!!))^(1/2),{i,1,Nmax}];
\[Phi]r=Table[Nnl[[i]]*r^L*E^(-vn[[i]]*r^2),{i,1,Nmax}];
wavef=Interpolation[data,Method->"Spline"];
If[Flag=="Debug",Print["h is ",h,"\n vn is :",vn,"\n Eij is ",Eij]];
norm=(NIntegrate[wavef[r]^2,{r,h,Length[data]*h}])^(-1/2);
FOverlapGBF=Table[NIntegrate[\[Phi]r[[i]]*r*wavef[r]*norm,{r,h,Length[data]*h}],{i,1,Nmax}];
coeffGBF=N[Inverse[Eij].FOverlapGBF];
SumGaussian=Sum[r*\[Phi]r[[i]]*coeffGBF[[i]],{i,1,Nmax}];
If[Flag=="Show",Plot[{SumGaussian,norm*wavef[r]},{r,h,Length[data]*h},PlotLegends->"Expressions"],coeffGBF]];

GetMGECoeff[path_,QNpar_]:=Block[{data,filename,wave,coeff,name},
wave=Switch[QNpar["L"],0,"S",1,"P",2,"D",3,"F"];
name=If[LetterQ[Characters[QNpar["Name"]][[2]]],ToUpperCase[StringJoin@@Characters[QNpar["Name"]][[1;;2]]],ToUpperCase[StringJoin@@Characters[QNpar["Name"]][[1]]]];
filename=path<>TemplateApply[name<>"`a``b``c``d`.dat",<|"a"->QNpar["N"]+1,"b"->2*QNpar["S"]+1,"c"->wave,"d"->QNpar["J"] |>];
If[FileExistsQ[filename],Continue,Print["file not exist :",filename];Return[0]];
If[FileExistsQ[filename<>"_ciL"],coeff=Import[filename<>"_ciL"],data=Import[filename];
coeff=FitMGECoeff[data,QNpar];Export[filename<>"_ciL",coeff,"Table"]];coeff];

MesonSpaceWavef[QNpar_,ML_,p_]:=Block[{n,J,L,S,type,\[Beta]},{n,J,L,S,type}={QNpar["N"],QNpar["J"],QNpar["L"],QNpar["S"],QNpar["type"]};<|"symbol"->TemplateApply["Psi(`L`,`ML`)",<|"L"->L,"ML"->ML|>],
"value"->Switch[type,"SHO",{psiMCart[QNpar["\[Beta]"],n,L,ML,p]},"MGE",Table[QNpar["ciL"][[i]]*MGEp[QNpar,p][[i]],{i,QNpar["Nmax"]}]*(I)^L*ShpCart[L,ML,p],_,0]|>];(*FIXME the factor (-i)^L need to check!!!*)

LBSpaceWavef[QNpar_,ML_,S3_,p__]:=Block[{n,J,L,S,type,\[Beta]\[Rho],\[Beta]\[Lambda]},{n,J,L,S,type}={QNpar["N"],QNpar["J"],QNpar["L"],QNpar["S"],QNpar["type"]};<|"symbol"->TemplateApply["Psi`S3`(`L`,`ML`)",<|"S3"->S3,"L"->L,"ML"->ML|>],
"value"->Switch[type,"SHO",{psiBCart[n,L,ML,S3,QNpar["\[Beta]\[Rho]"],QNpar["\[Beta]\[Lambda]"],p]},"MGE",{"FIXME"},_,0]|>];


(* ::Subsection:: *)
(*\:5473\:9053\:6ce2\:51fd\:6570*)


MesonFlavorWavef[Name_]:=<|"symbol"->TemplateApply["\[Phi](`a`)",<|"a"->Name|>],"value"->Switch[StringSplit[Name,{"(","_","^"}][[1]],
"K+",{{1,"uz"}},
"K-",{{1,"sx"}},
"K0",{{1,"dz"}},
"K0bar",{{1,"sy"}},
"\[Pi]+"|"\[Rho]+",{{1,"uy"}},
"\[Pi]-"|"\[Rho]-",{{1,"dx"}},
"\[Pi]0"|"\[Rho]0",{{1/Sqrt[2],"ux"},{-1/Sqrt[2],"dy"}},
"\[Eta]1",{{1/Sqrt[3],"ux"},{1/Sqrt[3],"dy"},{1/Sqrt[3],"sz"}},
"\[Eta]8",{{1/Sqrt[6],"ux"},{1/Sqrt[6],"dy"},{-2/Sqrt[6],"sz"}},
"\[Omega]"|"\[Eta]q",{{1/Sqrt[2],"ux"},{1/Sqrt[2],"dy"}},
"\[Phi]"|"\[Eta]s",{{1,"sz"}},
"D0",{{1,"cx"}},
"D0bar",{{1,"uv"}},
"D+",{{1,"cy"}},
"D-",{{1,"dv"}},
"Ds+",{{1,"cz"}},
"Ds-",{{1,"sv"}},
"\[CapitalPsi]",{{1,"cv"}},
"\[CapitalUpsilon]",{{1,"bw"}},
"B+",{{1,"uw"}},
"B-",{{1,"bx"}},
"B0",{{1,"dw"}},
"B0bar",{{1,"by"}},
"Bs0",{{1,"sw"}},
"Bs0bar",{{1,"bz"}},
"Bc+",{{1,"cw"}},
"Bc-",{{1,"bv"}},
"uubar",{{1,"ux"}},
"ddbar",{{1,"dy"}},
"ssbar",{{1,"sz"}},
"3p0",{{1/Sqrt[3],"ux"},{1/Sqrt[3],"dy"},{1/Sqrt[3],"sz"}},
_,Message[HadronWaveF::flavor]]|>;
LBFlavorWavef[S3_,Name_]:=<|"symbol"->TemplateApply["\[Phi](`a``b`)",<|"a"->S3,"b"->Name|>],"value"-> Switch[{S3,StringSplit[Name,{"(","_","^"}][[1]]},
{"S","\[CapitalDelta]++"},{{1,"uuu"}},
{"S","\[CapitalDelta]+"},{{1/Sqrt[3],"uud"},{1/Sqrt[3],"udu"},{1/Sqrt[3],"duu"}},
{"S","\[CapitalDelta]0"},{{1/Sqrt[3],"udd"},{1/Sqrt[3],"dud"},{1/Sqrt[3],"ddu"}},
{"S","\[CapitalDelta]-"},{{1,"ddd"}},
{"S","\[CapitalSigma]+"},{{1/Sqrt[3],"uus"},{1/Sqrt[3],"usu"},{1/Sqrt[3],"suu"}},
{"S","\[CapitalSigma]0"},{{1/Sqrt[6],"uds"},{1/Sqrt[6],"sud"},{1/Sqrt[6],"dsu"},{1/Sqrt[6],"sdu"},{1/Sqrt[6],"dus"},{1/Sqrt[6],"usd"}},
{"S","\[CapitalSigma]-"},{{1/Sqrt[3],"dds"},{1/Sqrt[3],"dsd"},{1/Sqrt[3],"sdd"}},
{"S","\[CapitalXi]0"},{{1/Sqrt[3],"uss"},{1/Sqrt[3],"sus"},{1/Sqrt[3],"ssu"}},
{"S","\[CapitalXi]-"},{{1/Sqrt[3],"dss"},{1/Sqrt[3],"sds"},{1/Sqrt[3],"ssd"}},
{"S","\[CapitalOmega]"},{{1,"sss"}},

{"\[Lambda]","N+"|"p"},{{2/Sqrt[6],"uud"},{-(1/Sqrt[6]),"duu"},{-(1/Sqrt[6]),"udu"}},
{"\[Lambda]","N0"|"n"},{{1/Sqrt[6],"dud"},{1/Sqrt[6],"udd"},{-(2/Sqrt[6]),"ddu"}},
{"\[Lambda]","\[CapitalSigma]+"},{{2/Sqrt[6],"uus"},{-(1/Sqrt[6]),"suu"},{-(1/Sqrt[6]),"usu"}},
{"\[Lambda]","\[CapitalSigma]0"},{{1/(2Sqrt[3]),"sdu"},{1/(2Sqrt[3]),"sud"},{1/(2Sqrt[3]),"usd"},{1/(2Sqrt[3]),"dsu"},{-2/(2Sqrt[3]),"uds"},{-2/(2Sqrt[3]),"dus"}},
{"\[Lambda]","\[CapitalSigma]-"},{{2/Sqrt[6],"dds"},{-(1/Sqrt[6]),"sdd"},{-(1/Sqrt[6]),"dsd"}},
{"\[Lambda]","\[CapitalLambda]"},{{1/2,"sud"},{1/2,"usd"},{-1/2,"sdu"},{-1/2,"dsu"}},
{"\[Lambda]","\[CapitalXi]0"},{{1/Sqrt[6],"sus"},{1/Sqrt[6],"uss"},{-2/Sqrt[6],"ssu"}},
{"\[Lambda]","\[CapitalXi]-"},{{1/Sqrt[6],"sds"},{1/Sqrt[6],"dss"},{-2/Sqrt[6],"ssd"}},
{"\[Rho]","N+"|"p"},{{1/Sqrt[2],"udu"},{-(1/Sqrt[2]),"duu"}},
{"\[Rho]","N0"|"n"},{{1/Sqrt[2],"udd"},{-(1/Sqrt[2]),"dud"}},
{"\[Rho]","\[CapitalSigma]+"},{{1/Sqrt[2],"usu"},{-(1/Sqrt[2]),"suu"}},
{"\[Rho]","\[CapitalSigma]0"},{{1/Sqrt[2],"sud"},{1/Sqrt[2],"sdu"},{-1/Sqrt[2],"usd"},{-(1/Sqrt[2]),"dsu"}},
{"\[Rho]","\[CapitalSigma]-"},{{1/Sqrt[2],"dsd"},{-(1/Sqrt[2]),"sdd"}},
{"\[Lambda]","\[CapitalLambda]"},{{1/(2Sqrt[3]),"usd"},{1/(2Sqrt[3]),"sdu"},{2/(2Sqrt[3]),"uds"},{-1/(2Sqrt[3]),"sud"},{-1/(2Sqrt[3]),"dsu"},{-2/(2Sqrt[3]),"dus"}},
{"\[Rho]","\[CapitalXi]0"},{{1/Sqrt[2],"uss"},{-(1/Sqrt[2]),"sus"}},
{"\[Rho]","\[CapitalXi]-"},{{1/Sqrt[2],"dss"},{-(1/Sqrt[2]),"sds"}},
_,Message[HadronWaveF::flavor];0
]|>;


(* ::Subsection:: *)
(*\:81ea\:65cb\:6ce2\:51fd\:6570*)


LBSpinWavef[S3_,S_,MS_]:=<|"symbol"->TemplateApply["\[Chi](`a``b``c`)",<|"a"->S3,"b"->S,"c"-> MS|>],"value"->Switch[{S3,S,MS},
{"S",3/2,3/2},{{1,"uuu"}},
{"S",3/2,1/2},{{1/Sqrt[3],"uud"},{1/Sqrt[3],"udu"},{1/Sqrt[3],"duu"}},
{"S",3/2,-(1/2)},{{1/Sqrt[3],"udd"},{1/Sqrt[3],"dud"},{1/Sqrt[3],"ddu"}},
{"S",3/2,-(3/2)},{{1,"ddd"}},
{"\[Lambda]",1/2,1/2},{{-1/Sqrt[6],"udu"},{-1/Sqrt[6],"duu"},{2/Sqrt[6],"uud"}},
{"\[Lambda]",1/2,-(1/2)},{{1/Sqrt[6],"udd"},{1/Sqrt[6],"dud"},{-2/Sqrt[6],"ddu"}},
{"\[Rho]",1/2,1/2},{{1/Sqrt[2],"udu"},{-1/Sqrt[2],"duu"}},
{"\[Rho]",1/2,-(1/2)},{{1/Sqrt[2],"udd"},{-1/Sqrt[2],"udu"}},_,0]|>;
MesonSpinWavef[S_,MS_]:=<|"symbol"->TemplateApply["\[Chi](`a``b`)",<|"a"->S,"b"->MS|>],"value"->Switch[{S,MS},
{1,1},{{1,"uu"}},
{1,0},{{1/Sqrt[2],"ud"},{1/Sqrt[2],"du"}},
{1,-1},{{1,"dd"}},
{0,0},{{1/Sqrt[2],"ud"},{-1/Sqrt[2],"du"}},
_,0
]|>;


(* ::Subsection:: *)
(*\:81ea\:65cb\:5473\:9053\:8026\:5408\:6ce2\:51fd\:6570*)


MesonSpinFlavorWavef[QNpar_,MS_]:=Block[{S,Name},{S,Name}={QNpar["S"],QNpar["Name"]};{{1,MesonFlavorWavef[Name],MesonSpinWavef[S,MS]}}];
LBSpinFlavorWavef[QNpar_,MS_,S3_]:=Block[{N6,N3,S,Name},{N6,N3,S,Name}={QNpar["N6"],QNpar["N3"],QNpar["S"],QNpar["Name"]};
Switch[{N6,N3,2*S+1,S3},
{56,8,2,"S"},{{1/Sqrt[2],LBFlavorWavef["\[Rho]",Name],LBSpinWavef["\[Rho]",S,MS]},{1/Sqrt[2],LBFlavorWavef["\[Lambda]",Name],LBSpinWavef["\[Lambda]",S,MS]}},
{56,10,4,"S"},{{1,LBFlavorWavef["S",Name],LBSpinWavef["S",S,MS]}},
{70,8,2,"\[Rho]"},{{1/Sqrt[2],LBFlavorWavef["\[Rho]",Name],LBSpinWavef["\[Lambda]",S,MS]},{1/Sqrt[2],LBFlavorWavef["\[Lambda]",Name],LBSpinWavef["\[Rho]",S,MS]}},
{70,8,4,"\[Rho]"},{{1,LBFlavorWavef["\[Rho]",Name],LBSpinWavef["S",S,MS]}},
{70,10,2,"\[Rho]"},{{1,LBFlavorWavef["S",Name],LBSpinWavef["\[Rho]",S,MS]}},
{70,1,2,"\[Rho]"},{{1,LBFlavorWavef["\[Alpha]",Name],LBSpinWavef["\[Lambda]",S,MS]}},
{70,8,2,"\[Lambda]"},{{1/Sqrt[2],LBFlavorWavef["\[Rho]",Name],LBSpinWavef["\[Rho]",S,MS]},{-(1/Sqrt[2]),LBFlavorWavef["\[Lambda]",Name],LBSpinWavef["\[Lambda]",S,MS]}},
{70,8,4,"\[Lambda]"},{{1,LBFlavorWavef["\[Lambda]",Name],LBSpinWavef["S",S,MS]}},
{70,10,2,"\[Lambda]"},{{1,LBFlavorWavef["S",Name],LBSpinWavef["\[Lambda]",S,MS]}},
{70,1,2,"\[Lambda]"},{{1,LBFlavorWavef["\[Alpha]",Name],LBSpinWavef["\[Rho]",S,MS]}},
{20,8,2,"\[Alpha]"},{{1/Sqrt[2],LBFlavorWavef["\[Rho]",Name],LBSpinWavef["\[Lambda]",S,MS]},{-(1/Sqrt[2]),LBFlavorWavef["\[Lambda]",Name],LBSpinWavef["\[Rho]",S,MS]}},{20,1,4,"\[Alpha]"},{{1,LBFlavorWavef["\[Alpha]",Name],LBSpinWavef["S",S,MS]}},
_,0]];


(* ::Subsubsection:: *)
(*\:603b\:6ce2\:51fd\:6570*)


LBWavef[QNpar_,MJ_,ML_,MS_,p__]:=Block[{n6,n,j,l,s,Name,n3},{n6,n3,n,j,l,s,Name}={QNpar["N6"],QNpar["N3"],QNpar["N"],QNpar["J"],QNpar["L"],QNpar["S"],QNpar["Name"]};
Switch[n6,
56,{{ClebschGordan[{l,ML},{s,MS},{j,MJ}],LBSpinFlavorWavef[QNpar,MS,"S"],LBSpaceWavef[QNpar,ML,"S",p]}},
70,{{1/Sqrt[2] ClebschGordan[{l,ML},{s,MS},{j,MJ}],LBSpinFlavorWavef[QNpar,MS,"\[Rho]"],LBSpaceWavef[QNpar,ML,"\[Rho]",p]},{1/Sqrt[2] ClebschGordan[{l,ML},{s,MS},{j,MJ}],LBSpinFlavorWavef[QNpar,MS,"\[Lambda]"],LBSpaceWavef[QNpar,ML,"\[Lambda]",p]}},_,0]];
MesonWavef[QNpar_,MJ_,ML_,MS_,p__]:=Block[{n,j,l,s,Name},{n,j,l,s,Name}={QNpar["N"],QNpar["J"],QNpar["L"],QNpar["S"],QNpar["Name"]};
{{ClebschGordan[{l,ML},{s,MS},{j,MJ}],MesonSpinFlavorWavef[QNpar,MS],MesonSpaceWavef[QNpar,ML,p]}}];
QNCop[m_,p__]:={{ClebschGordan[{1,m},{1,-m},{0,0}],MesonSpinFlavorWavef[<|"S"-> 1,"Name"-> "3p0"|>,-m],<|"symbol"->TemplateApply["Psi(1`ML`)",<|"ML"->m|>],"value"->{ShpCart[1,m,p]}|>}}


End[]
EndPackage[]
