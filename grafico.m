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
	"GraficoFunzione[f, x, limite0, limite1] disegna il grafico della funzione f
	tra gli estremi limite0 e limite1."

Begin["`Private`"]

GraficoFunzione[f_, x_, limite0_, limite1_]:=Plot[f[x],{x, limite0, limite1}]; 

End[]
EndPackage[]
