(* ::Package:: *)

(* :Title: Limiti *)

(* :Context: Limiti`*)

(* :Author: Sebastian Davrieux *)

(* :Summary:
   Versione preliminare di funzione per il calcolo dei limiti agli estremi della funzione
 *)

(* :Copyright: Davrieux *)

(* :Package Version:  *)

(* :Mathematica Version: *)

(* :History:  *)

(* :Sources:  biblio   *)

(* :Limitations:   *)

(* :Discussion:   *)

BeginPackage["Limiti`"]

CreazioneListaDominio::usage =
	"CreazioneListaDominio[listaDominio,dominio] crea la lista da essere usata per chiamare le altre funzioni."

CalcolaEstremiInferiori::usage =
	"CalcolaEstremiInferiori[estremiInferiori,dominio] calcola gli estremi inferiori del dominio."

CalcolaEstremiSuperiori::usage =
	"CalcolaEstremiSuperiori[estremiSuperiori,dominio] calcola gli estremi superiori del dominio."

CalcolaLimiti::usage =
	"CalcolaLimiti[f,x] calcola il dominio."

Begin["`Private`"]

ControlloSeLista[var_]:=(
	If[ListQ[var],var,{var}]
);

SetAttributes[CreazioneListaDominio,HoldFirst];
CreazioneListaDominio[listaDominio_,dominio_]:=(listaDominio=Union[listaDominio,ControlloSeLista[Select[dominio,Head[#]==Greater&]]];
	listaDominio=Union[listaDominio,ControlloSeLista[Select[dominio,Head[#]==GreaterEqual&]]];
	listaDominio=Union[listaDominio,ControlloSeLista[Select[dominio,Head[#]==Inequality&]]];
	listaDominio=Union[listaDominio,ControlloSeLista[Select[dominio,Head[#]==Less&]]];
	listaDominio=Union[listaDominio,ControlloSeLista[Select[dominio,Head[#]==LessEqual&]]];
	listaDominio = DeleteCases[listaDominio,False];
);

SetAttributes[CalcolaEstremiInferiori,HoldFirst];
CalcolaEstremiInferiori[estremiInferiori_,dominio_]:=(estremiInferiori=Union[estremiInferiori,#[[2]]&/@Select[dominio,Head[#]==Greater&]];
	estremiInferiori=Union[estremiInferiori,#[[2]]&/@Select[dominio,Head[#]==GreaterEqual&]];
	estremiInferiori=Union[estremiInferiori,If[#[[2]]===Less,#[[1]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiInferiori=Union[estremiInferiori,If[#[[2]]===LessEqual,#[[1]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiInferiori=Union[estremiInferiori,If[#[[2]]===Greater,#[[5]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiInferiori=Union[estremiInferiori,If[#[[2]]===GreaterEqual,#[[5]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiInferiori=DeleteCases[estremiInferiori,Null];
);

SetAttributes[CalcolaEstremiSuperiori,HoldFirst];
CalcolaEstremiSuperiori[estremiSuperiori_,dominio_]:=(estremiSuperiori=Union[estremiSuperiori,#[[2]]&/@Select[dominio,Head[#]==Less&]];
	estremiSuperiori=Union[estremiSuperiori,#[[2]]&/@Select[dominio,Head[#]==LessEqual&]];
	estremiSuperiori=Union[estremiSuperiori,If[#[[2]]===Less,#[[5]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiSuperiori=Union[estremiSuperiori,If[#[[2]]===LessEqual,#[[5]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiSuperiori=Union[estremiSuperiori,If[#[[2]]===Greater,#[[1]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiSuperiori=Union[estremiSuperiori,If[#[[2]]===GreaterEqual,#[[1]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiSuperiori=DeleteCases[estremiSuperiori,Null];
);

CalcolaLimiti[f_,x_]:=(
	dominio =FunctionDomain[f[x],x];
	dominio=If[Head[dominio]===Or,dominio,{dominio}];

	listaDominio={};
	CreazioneListaDominio[listaDominio,dominio];
	listaDominio;

	Print["quindi gli estremi inferiori sono: "];
	estremiInferiori={};
	CalcolaEstremiInferiori[estremiInferiori,listaDominio];
	Print[estremiInferiori];

	Print["quindi gli estremi superiori sono: "];
	estremiSuperiori = {};
	CalcolaEstremiSuperiori[estremiSuperiori,listaDominio];
	Print[estremiSuperiori];

	calcolaInfinito=If[Max[estremiInferiori]>= Max[estremiSuperiori],True,False];
	calcolaMenoInfinito=If[Min[estremiInferiori]>= Min[estremiSuperiori],True,False];
	If[calcolaMenoInfinito,(Print["Il limite per x->",-\[Infinity]," nell'intorno destro = ",Limit[f[x],x->-Infinity],If[NumberQ[Limit[f[x],x->-Infinity]]," quindi \[EGrave] un asintoto orizzontale.", " quindi \[EGrave] un asintoto verticale."]])]
	If[calcolaInfinito,(Print["Il limite per x->",+\[Infinity]," nell'intorno sinistro = ",Limit[f[x],x->Infinity],If[NumberQ[Limit[f[x],x->Infinity]]," quindi \[EGrave] un asintoto orizzontale.", " quindi \[EGrave] un asintoto verticale."]])]
	(Print["Il limite per x->",# ," nell'intorno destro = ",Limit[f[x],x->#,Direction->1],If[NumberQ[Limit[f[x],x->#,Direction->1]]," quindi \[EGrave] un asintoto orizzontale.", " quindi \[EGrave] un asintoto verticale."]])&/@estremiInferiori;
	(Print["Il limite per x->",# ," nell'intorno sinistro = ",Limit[f[x],x->#,Direction->-1],If[NumberQ[Limit[f[x],x->#,Direction->-1]]," quindi \[EGrave] un asintoto orizzontale.", " quindi \[EGrave] un asintoto verticale."]])&/@estremiSuperiori;
);

End[]
EndPackage[]



