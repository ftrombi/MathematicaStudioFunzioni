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

StampaConcavoConvessoNoSoluzioni::usage =
	"StampaConcavoConvessoNoSoluzioni[d2_] permette di stampare correttamente la concavit\[AGrave]
	o la convessit\[AGrave] di una funzione con derivata seconda senza soluzioni."

GraficoDerivataSeconda::usage =
	"GraficaDerivataSeconda[d2_, x_, limite0_, limite1_] permette di eseguire un plot
	della funzione tra gli intervalli limite0 e limite1"

StampaConcavoConvesso::usage =
	"StampaConcavoConvesso[intervalliConvessa_, intervalliConcava_] permette di
	stampare la convessit\[AGrave] e la concavit\[AGrave] della funzione con una formattazione corretta."

StampaIntervalli::usage =
	"StampaIntervalli[intervalliConvessa_, intervalliConcava_] permette di stampare
	gli intervalli di convessit\[AGrave] e concavit\[AGrave] della funzione con una formattazione corretta"

CalcoloDerivataSeconda::usage =
	"CalcoloDerivataSeconda[f, x] calcola la derivata seconda della funzione f."

SegnoDerivataSeconda::usage =
	"SegnoDerivataSeconda[f, x] restituisce il segno della derivata seconda della funzione f."

Begin["`Private`"]

StampaConcavoConvessoNoSoluzioni[d2_] := If[d2 > 0, Print["La funzione \[EGrave] convessa"],
	Print["La funzione \[EGrave] concava"]];

GraficoDerivataSeconda[d_, x_, limite0_, limite1_] := (
	Print["Il grafico della derivata seconda \[EGrave] il seguente:"];
	Plot[d, {x, -5,+5}]
);

StampaConcavoConvesso[intervalliConvessa_, intervalliConcava_] :=
  If[Length[intervalliConvessa] > 0, 
   Print["La funzione \[EGrave] convessa."], 
   Print["La funzione \[EGrave] concava"]];

StampaIntervalli[intervalliConvessa_, intervalliConcava_]:=(
	Print["Gli intervalli in cui la derivata seconda \[EGrave] positiva e dunque la funzione \[EGrave] convessa sono:"];
	Print[intervalliConvessa];
	Print["Gli intervalli in cui la derivata seconda \[EGrave] negativa e dunque la funzione \[EGrave] concava sono:"];
	Print[intervalliConcava];

);

CalcoloDerivataSeconda[f_, x_]:= D[D[f[x], x], x];

SegnoDerivataSeconda[f_, x_] := (
    d2 = CalcoloDerivataSeconda[f, x];
	num = Numerator[d2];
	soluzioni = NSolve[num == 0, x];
	intervalliConvessa = Reduce[d2 > 0, x];
	intervalliConcava = Reduce[d2 < 0, x];
	If[Length[intervalliConvessa] == 0 || Length[intervalliConcava] == 0,
		StampaConcavoConvessoNoSoluzioni[d2], StampaIntervalli[intervalliConvessa, intervalliConcava]];
	(*If[StringCount[ToString[intervalliConvessa], "False"] > 0 || 
  	StringCount[ToString[intervalliConcava], "False"] > 0,
  	StampaConcavoConvesso[intervalliConvessa, intervalliConcava],
	  StampaIntervalli[intervalliConvessa, intervalliConcava]];*)
)

End[]
EndPackage[]
