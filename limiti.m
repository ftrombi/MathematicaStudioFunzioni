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

BeginPackage["Dominio`"]

CreazioneListaDominio::usage =
	"CreazioneListaDominio[listaDominio,dominio] crea la lista da essere usata per chiamare le altre funzioni."

CalcolaEstremiInferiori::usage =
	"CalcolaEstremiInferiori[estremiInferiori,dominio] calcola gli estremi inferiori del dominio."

CalcolaEstremiSuperiori::usage =
	"CalcolaEstremiSuperiori[estremiSuperiori,dominio] calcola gli estremi superiori del dominio."

Begin["`Private`"]

SetAttributes[CreazioneListaDominio,HoldFirst];
CreazioneListaDominio[listaDominio_,dominio_]:=(listaDominio=Union[listaDominio,{Select[dominio,Head[#]==Greater&]}];
	listaDominio=Union[listaDominio,{Select[dominio,Head[#]==GreaterEqual&]}];
	listaDominio=Union[listaDominio,{Select[dominio,Head[#]==Inequality&]}];
	listaDominio=Union[listaDominio,{Select[dominio,Head[#]==Less&]}];
	listaDominio=Union[listaDominio,{Select[dominio,Head[#]==LessEqual&]}];
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

End[]
EndPackage[]



