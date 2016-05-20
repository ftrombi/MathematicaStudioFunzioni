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
	la funzione f \[EGrave] positiva."

Begin["`Private`"]

SegnoFunzione[f_, x_]:= (
	valore = Reduce[f[x] > 0, x, Reals];
	If[valore==False, Print["La funzione \[EGrave] negativa."],
	Print["La funzione \[EGrave] positiva per " <> ToString[valore]]];
); 

End[]
EndPackage[]
