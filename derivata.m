(* ::Package:: *)

(* :Title: DerivataPrima *)

(* :Context: DerivataPrima` *)

(* :Author: Francesco Trombi *)

(* :Summary:
   Versione preliminare di funzione per la gestione della derivata prima
   all'interno di uno studio di funzione
 *)

(* :Copyright: Trombi *)

(* :Package Version:  *)

(* :Mathematica Version: *)

(* :History:  *)

(* :Sources:  biblio   *)

(* :Limitations:   *)

(* :Discussion:   *)

BeginPackage["DerivataPrima`"]

CalcoloDerivataPrima::usage =
	"CalcoloDerivataPrima[f, x] calcola la derivata prima della funzione f."

Begin["`Private`"]

CalcoloDerivataPrima[f_, x_]:= Simplify[D[f[x], x]];

End[]
EndPackage[]
