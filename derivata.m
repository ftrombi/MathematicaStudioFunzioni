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

SegnoDerivataPrima::usage =
	"SegnoDerivataPrima[f, x] restituisce il segno della derivata prima della funzione f."

Begin["`Private`"]

CalcoloDerivataPrima[f_, x_]:= Simplify[D[f[x], x]];

SegnoDerivataPrima[f_, x_] := (
  d = CalcoloDerivataPrima[f, x];
	num = Numerator[d];
	soluzioni = NSolve[num == 0, x];
	punto = Reduce[d == 0, x];
	segno1 = Reduce[d < 0, x];
	segno2 = Reduce[d > 0, x];
	If[StringCount[ToString[segno1], "False"] > 0 || 
  StringCount[ToString[segno2], "False"] > 
   0, "La funzione è monotona", segno1 segno2]
)

End[]
EndPackage[]
