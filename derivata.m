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

StampaMonotonia::usage =
	"StampaMonotonia[intervalliPositivi, intervalliNegativi] permette di stampare la monotonia
	della funzione con una formattazione corretta"

StampaIntervalli::usage =
	"StampaIntervalli[listaIntervalliPositivi, listaIntervalliNegativi] permette di stampare
	gli intervalli di monotonia della funzione con una formattazione corretta"

CalcoloDerivataPrima::usage =
	"CalcoloDerivataPrima[f, x] calcola la derivata prima della funzione f."

SegnoDerivataPrima::usage =
	"SegnoDerivataPrima[f, x] restituisce il segno della derivata prima della funzione f."

Begin["`Private`"]

StampaMonotonia[intervalliPositivi_, intervalliNegativi_] :=
  If[Length[intervalliPositivi] > 0, 
   Print["La funzione \[EGrave] monotona crescente"], 
   Print["La funzione \[EGrave] monotona decrescente"]];

StampaIntervalli[listaIntervalliPositivi_, listaIntervalliNegativi_]:=(
	Print["Gli intervalli in cui la derivata \[EGrave] positiva sono:"];
	Print[listaIntervalliPositivi];
	Print["Gli intervalli in cui la derivata \[EGrave] negativa sono:"];
	Print[listaIntervalliNegativi];

);

CalcoloDerivataPrima[f_, x_]:= Simplify[D[f[x], x]];

SegnoDerivataPrima[f_, x_] := (
    d = CalcoloDerivataPrima[f, x];
	Print["La derivata prima della funzione \[EGrave]:"];
	Print[d];
	num = Numerator[d];
	soluzioni = Solve[num == 0, x];
	Print["Le soluzioni della derivata prima sono:"];
	Print[Column[soluzioni]];
	intervalliPositivi = Reduce[d > 0, x];
	intervalliNegativi = Reduce[d < 0, x];
	If[StringCount[ToString[intervalliPositivi], "False"] > 0 || 
      StringCount[ToString[intervalliNegativi], "False"] > 0, 
      StampaMonotonia[intervalliPositivi, intervalliNegativi], 
      StampaIntervalli[intervalliPositivi, intervalliNegativi]];
)

End[]
EndPackage[]
