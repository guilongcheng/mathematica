(* ::Package:: *)

BeginPackage["QuarkModel`"]
Unprotect@@Names["QuarkModel`*"];
ClearAll@@Names["QuarkModel`*"];
PDGdata::usage="PDGdata[Name] PDG\:4e0a\:7ed9\:51fa\:7684\:7c92\:5b50\:4fe1\:606f";
TranToAs::usage="TranToAs[Mass,N,J,L,S] \:5c06\:53c2\:6570\:8f6c\:6362\:6210Association";


Begin["`Private`"]
PDGdatainfo::particle="This particle `1` can't find!";
TranToAs[Name_,Mass_,N_,J_,L_,S_,N6_:Null,N3_:Null]:=<|"Name"->Name,"Mass"->Mass,"N"->N,"J"-> J,"L"-> L,"S"-> S,"N6"->N6,"N3"->N3|>;
PDGdata[Name_]:=Switch[Name,
"K0_1(1270)"|"K0bar_1(1270)"|"K+_1(1270)"|"K-_1(1270)",    TranToAs[Name,1.270,0,1,1,1],
"K0_3p1(1270)"|"K0bar_3p1(1270)", TranToAs[Name,1.270,0,1,1,1],
"K0_1p1(1270)"|"K0bar_1p1(1270)", TranToAs[Name,1.270,0,1,1,0],
"K0_1(1400)"|"K0bar_1(1400)"|"K+_1(1400)"|"K-_1(1400)",    TranToAs[Name,1.403,0,1,1,0],
"K0_3p1(1400)"|"K0bar_3p1(1400)", TranToAs[Name,1.403,0,1,1,1],
"K0_1p1(1400)"|"K0bar_1p1(1400)", TranToAs[Name,1.403,0,1,1,0],
"K0^*(1410)"|"K0bar^*(1410)"|"K+^*(1410)"|"K-^*(1410)",    TranToAs[Name,1.421,1,1,0,1],
"K0_0^*(1430)"|"K0bar_0^*(1430)"|"K+_0^*(1430)"|"K-_0^*(1430)",    TranToAs[Name,1.425,0,0,1,1],
"K0_2^*(1430)"|"K0bar_2^*(1430)",                          TranToAs[Name,1.4324,0,2,1,1],
"K+_2^*(1430)"|"K-_2^*(1430)",                             TranToAs[Name,1.4256,0,2,1,1],
"K0^*(1680)"|"K0bar^*(1680)"|"K+^*(1680)"|"K-^*(1680)",    TranToAs[Name,1.718,0,1,2,1],
"\[Pi]0",        TranToAs[Name,0.134977,0,0,0,0],
"\[Pi]+"|"\[Pi]-",        TranToAs[Name,0.13957,0,0,0,0],
"\[Eta]1"|"\[Eta]q",        TranToAs[Name,0.547862,0,0,0,0],
"K0"|"K0bar",TranToAs[Name,0.497611,0,0,0,0],
"K+"|"K-",   TranToAs[Name,0.493677,0,0,0,0],
"\[Eta]8"|"\[Eta]s",        TranToAs[Name,0.95778,0,0,0,0],
"\[Rho]0",        TranToAs[Name,0.769,0,1,0,1],
"\[Rho]+"|"\[Rho]-",        TranToAs[Name,0.7665,0,1,0,1],
"K0^*"|"K0bar^*",    TranToAs[Name,0.89555,0,1,0,1],
"K+^*"|"K-^*",    TranToAs[Name,0.89176,0,1,0,1],
"\[Omega]",    TranToAs[Name,0.78265,0,1,0,1],
"\[Phi]",    TranToAs[Name,1.019461,0,1,0,1],
"B0bar"|"B0"|"B+"|"B-", TranToAs[Name,5.279,0,0,0,0],
"B0bar^*"|"B0^*"|"B+^*"|"B-_1", TranToAs[Name,5.325,0,1,0,1],
"Bs0bar"|"Bs0", TranToAs[Name,5.367,0,0,0,0],
"Bs0bar^*"|"Bs0^*", TranToAs[Name,5.415,0,1,0,1],
"D0"|"D0bar"|"D+"|"D-", TranToAs[Name,1.870,0,0,0,0],
"D0^*"|"D0bar^*"|"D+^*"|"D-^*", TranToAs[Name,2.010,0,1,0,1],
"D0_0^*"|"D0bar_0^*"|"D+_0^*"|"D-_0^*", TranToAs[Name,2.252,0,0,1,1],
"D0_1p1"|"D-_1p1",TranToAs[Name,Null,0,1,1,0],
"D0_3p1"|"D-_3p1",TranToAs[Name,Null,0,1,1,1],
"Ds+"|"Ds-", TranToAs[Name,1.968,0,0,0,0],
"Ds+^*"|"Ds-^*", TranToAs[Name,2.112,0,1,0,1],
"\[CapitalSigma]0",      TranToAs[Name,1.192642,0,1/2,0,1/2,56,8],
"\[CapitalSigma]+",      TranToAs[Name,1.18937,0,1/2,0,1/2,56,8],
"\[CapitalSigma]-",      TranToAs[Name,1.197449,0,1/2,0,1/2,56,8],
"\[CapitalXi]-",      TranToAs[Name,1.32171,0,1/2,0,1/2,56,8],
"\[CapitalXi]0",      TranToAs[Name,1.31486,0,1/2,0,1/2,56,8],
"\[CapitalXi]0(1530)",TranToAs[Name,1.5318,0,3/2,0,3/2,56,10],
"\[CapitalXi]-(1530)",TranToAs[Name,1.535,0,3/2,0,3/2,56,10],
_,Message[PDGdatainfo::particle,Name];0
];
End[]
EndPackage[]
