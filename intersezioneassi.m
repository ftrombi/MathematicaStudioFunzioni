(* ::Package:: *)

(* :Title: Intersezione Assi *)

(* :Context: IntersezioneAssi`*)

(* :Author: Sebastian Davrieux *)

(* :Summary:
   Versione preliminare di funzione per il calcolo dell'intersezione con gli assi
 *)

(* :Copyright: Davrieux *)

(* :Package Version:  *)

(* :Mathematica Version: *)

(* :History:  *)

(* :Sources:  biblio   *)

(* :Limitations:   *)

(* :Discussion:   *)

BeginPackage["IntersezioneAssi`"]

ControlloIntersezioneAssi::usage = 
	"ControlloIntersezioneAssi[f,x] calcolo l'intersezione con gli assi"

Begin["`Private`"]

ControlloIntersezioneAssi[f_,x_]:=(
	Print["Per scoprire dove interseca l'asse x poniamo il numeratore = 0: "];
	Print[Numerator[f[x]]==0];
	Print["che risulta: "];
	risultatoIntersezioneX=Reduce[f[x]==0,x,Reals];
	If[risultatoIntersezioneX===False,
	Print["falso, non esistono intersezioni con l'asse x."],
	Print[risultatoIntersezioneX]];
	Print["Per scoprire dove interseca l'asse y, calcoliamo f(x) con x=0 se il dominio lo ammette e otteniamo che interseca l'asse y in"];
	Print["y==",Numerator[f[x]]/.x->0];
);

End[]
EndPackage[]






