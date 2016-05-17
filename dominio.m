(* ::Package:: *)

(* :Title: Dominio *)

(* :Context: Dominio`*)

(* :Author: Sebastian Davrieux *)

(* :Summary:
   Versione preliminare di funzione per il calcolo del dominio
 *)

(* :Copyright: Davrieux *)

(* :Package Version:  *)

(* :Mathematica Version: *)

(* :History:  *)

(* :Sources:  biblio   *)

(* :Limitations:   *)

(* :Discussion:   *)

BeginPackage["Dominio`"]

VerificaRazionale::usage =
	"VerificaRazionale[x] controlla se x \[EGrave] un razionale."

PrendiContenutoRadice::usage =
	"PrendiContenutoRadice[x] restituisce il contenuto della radice."

PrendiContenutoLogaritmo::usage =
	"PrendiContenutoLogaritmo[x] restituisce il contenuto del logaritmo."

PrendiSecondoContenutoSeHeadUgualeTimes::usage =
	"PrendiSecondoContenutoSeHeadUgualeTimes[x] restituisce il contenuto di Times."

DividiFunzioneInListaDiOperandi::usage =
	"DividiFunzioneInListaDiOperandi[x] restituisce la lista di operandi che compongono una funzione."

CalcolaContenutiRadiciDaControllare::usage =
	"CalcolaContenutiRadiciDaControllare[listaContenutiRadiciDaControllare,x] restituisce una lista con i polinomi contenuti sotto radice."

CalcolaContenutiLogaritmoDaControllare::usage =
	"CalcolaContenutiLogaritmoDaControllare[listaContenutiLogaritmoDaControllare,x] restituisce una lista con i polinomi contenuti dentro i logaritmi."

Begin["`Private`"]

VerificaRazionale[x_]:=If[Simplify[x\[Element]Rationals],True,False,Null];

PrendiContenutoRadice[x_]:=If[Head[x] ==Power,
	If[VerificaRazionale[Part[x,2]],
		If[EvenQ[Denominator[Part[x,2]]],Part[x,1],Null,Null], 
		Null, 
		Null], 
	Null,
	Null];

PrendiContenutoLogaritmo[x_]:=If[Head[x] ==Log,Part[x,1],Null,Null];

PrendiSecondoContenutoSeHeadUgualeTimes[x_]:=If[Head[x] ==Times,Part[x,2],x,x];

DividiFunzioneInListaDiOperandi[x_] :=If[Denominator[x]===1,
	List @@ Numerator[x],
	Join[List @@ Denominator[x], List@@Numerator[x]]];

SetAttributes[CalcolaContenutiRadiciDaControllare,HoldFirst];
CalcolaContenutiRadiciDaControllare[listaContenutiRadiciDaControllare_,x_]:=If[PrendiContenutoRadice[PrendiSecondoContenutoSeHeadUgualeTimes[x]] ===Null,
	Null,
	listaContenutiRadiciDaControllare=Append[listaContenutiRadiciDaControllare,PrendiContenutoRadice[PrendiSecondoContenutoSeHeadUgualeTimes[x]]]];

SetAttributes[CalcolaContenutiLogaritmoDaControllare,HoldFirst];
CalcolaContenutiLogaritmoDaControllare[listaContenutiLogaritmoDaControllare_,x_]:=If[PrendiContenutoLogaritmo[PrendiSecondoContenutoSeHeadUgualeTimes[x]] ===Null,
	Null,
	listaContenutiLogaritmoDaControllare=Append[listaContenutiLogaritmoDaControllare,PrendiContenutoLogaritmo[PrendiSecondoContenutoSeHeadUgualeTimes[x]]]];

End[]
EndPackage[]



