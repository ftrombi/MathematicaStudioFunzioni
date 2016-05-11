(* ::Package:: *)

(* :Title: Grafico *)


(* :Context: Grafico` *)

(* :Author: Francesco Trombi *)

(* :Summary:
   Versione preliminare di funzione per il disegno del grafico di una funzione
 *)

(* :Copyright:  Trombi *)

(* :Package Version:  *)

(* :Mathematica Version: *)

(* :History:  *)

(* :Sources:  biblio   *)

(* :Limitations:   *)

(* :Discussion:   *)

BeginPackage["Grafico`"]

GraficoFunzione::usage =
	"GraficoFunzione[f, x, x0, x1] disegna il grafico della funzione f
	tra gli estremi x0 e x1."

Begin["`Private`"]

GraficoFunzione[f_, x_, x0_, x1_]:=Plot[f,{x, x0, x1}];

End[]
EndPackage[]
