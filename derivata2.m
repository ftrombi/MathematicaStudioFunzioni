(* ::Package:: *)

(* :Title: DerivatSeconda *)

(* :Context: DerivatSeconda` *)

(* :Author: Francesco Trombi *)

(* :Summary:
   Versione preliminare di funzione per la gestione della derivata seconda
   all'interno di uno studio di funzione
 *)

(* :Copyright: Trombi *)

(* :Package Version:  *)

(* :Mathematica Version: *)

(* :History:  *)

(* :Sources:  biblio   *)

(* :Limitations:   *)

(* :Discussion:   *)

BeginPackage["DerivatSeconda`"]

CalcoloDerivataSeconda::usage =
	"CalcoloDerivataSeconda[f, x] calcola la derivata seconda della funzione f."

SegnoDerivataSeconda::usage =
	"SegnoDerivataSeconda[f, x] restituisce il segno della derivata seconda della funzione f."

Begin["`Private`"]

CalcoloDerivataSeconda[f_, x_]:= D[D[f[x], x], x];

SegnoDerivataSeconda[f_, x_] := (
  d = CalcoloDerivataSeconda[f, x];
	num = Numerator[d];
	soluzioni = NSolve[num == 0, x];
	punto = Reduce[d == 0, x];
	segno1 = Reduce[d < 0, x];
	segno2 = Reduce[d > 0, x];
	If[StringCount[ToString[segno1], "False"] > 0 || 
  	StringCount[ToString[segno2], "False"] > 0,
  	"La funzione non cambia convessità", segno1 segno2]
)

End[]
EndPackage[]
