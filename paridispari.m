(* ::Package:: *)

(* :Title: PariDispari *)

(* :Context: PariDispari` *)

(* :Author: Francesco Trombi *)

(* :Summary:
   Versione preliminare di funzione per determinare la parit\[AGrave]
   o la disparit\[AGrave] di una funzione.
 *)

(* :Copyright: Trombi *)

(* :Package Version:  *)

(* :Mathematica Version: *)

(* :History:  *)

(* :Sources:  biblio   *)

(* :Limitations:   *)

(* :Discussion:   *)

BeginPackage["PariDispari`"]

ControlloPariDispari::usage =
	"ControlloPariDispari[f, x] determina se una funzione \[EGrave] pari o dispari."

Begin["`Private`"]

ControlloPariDispari[f_, x_]:= (
  risultatoPari = FullSimplify[ForAll[x, f[x] == f[-x]]];
  risultatoDispari = FullSimplify[ForAll[x, -f[x] == f[-x]]];
  If[risultatoPari, Print["La funzione \[EGrave] pari."], 
   Print["La funzione non \[EGrave] pari."]];
  If[risultatoDispari, Print["La funzione \[EGrave] dispari."], 
   Print["La funzione non \[EGrave] dispari."]];
  ); 

End[]
EndPackage[]
