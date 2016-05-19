(* ::Package:: *)

(* :Title: PariDispari *)

(* :Context: PariDispari` *)

(* :Author: Francesco Trombi *)

(* :Summary:
   Versione preliminare di funzione per determinare la parità
   o la disparità di una funzione.
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
	"ControlloPariDispari[f, x] determina se una funzione è pari o dispari."

Begin["`Private`"]

ControlloPariDispari[f_, x_]:= (
  risultatoPari = FullSimplify[ForAll[x, f[x] == f[-x]]];
  risultatoDispari = FullSimplify[ForAll[x, -f[x] == f[-x]]];
  If[risultatoPari == True, Print["La funzione è pari."], 
   Print["La funzione non è pari."]];
  If[risultatoDispari == True, Print["La funzione è dispari."], 
   Print["La funzione non è dispari."]];
  ); 

End[]
EndPackage[]
