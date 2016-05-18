(* ::Package:: *)

(* :Title: Segno *)

(* :Context: Segno` *)

(* :Author: Francesco Trombi *)

(* :Summary:
   Versione preliminare di funzione per il
   calcolo del segno della funzione
 *)

(* :Copyright: Trombi *)

(* :Package Version:  *)

(* :Mathematica Version: *)

(* :History:  *)

(* :Sources:  biblio   *)

(* :Limitations:   *)

(* :Discussion:   *)

BeginPackage["Segno`"]

SegnoFunzione::usage =
	"SegnoFunzione[f, x] determina da quale valore
	la funzione f è positiva."

Begin["`Private`"]

SegnoFunzione[f_, x_]:= (
	valore = N@Reduce[f[x] > 0, x];
	"La funzione è positiva per " <> ToString[valore]
); 

End[]
EndPackage[]
