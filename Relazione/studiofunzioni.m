(* ::Package:: *)

(* :Title: StudioFunzioni *)

(* :Context: StudioFunzioni` *)

(* :Author: Francesco Trombi e Sebastian Davrieux *)

(* :Summary:
   Insieme di funzioni per il supporto allo studio di funzioni nelle scuole superiori.
 *)

(* :Copyright: Trombi e Davrieux *)

(* :Package Version:  *)

(* :Mathematica Version: *)

(* :History:  *)

(* :Sources:  biblio   *)

(* :Limitations:   *)

(* :Discussion:   *)

BeginPackage["StudioFunzioni`"]

(* Dominio *)
CalcolaDominio::usage =
	"CalcolaDominio[risultatoControlloDenominatore,listaContenutiDenominatoreDaControllare] restituisce una lista con i polinomi contenuti dentro il denominatore."

(* Pari e dispari *)
ControlloPariDispari::usage =
	"ControlloPariDispari[f, x] determina se una funzione \[EGrave] pari o dispari."

(* Intersezione con gli assi *)
ControlloIntersezioneAssi::usage = 
	"ControlloIntersezioneAssi[f,x] calcolo l'intersezione con gli assi"

(* Segno funzione *)
SegnoFunzione::usage =
	"SegnoFunzione[f, x] determina da quale valore
	la funzione f \[EGrave] positiva."

(* Limiti e asintoti *)
CalcolaLimiti::usage =
	"CalcolaLimiti[f,x] calcola i limiti e gli asintoti."

(* Derivata Prima *)
CalcoloDerivataPrima::usage =
	"CalcoloDerivataPrima[f, x] calcola la derivata prima della funzione f."

SegnoDerivataPrima::usage =
	"SegnoDerivataPrima[f, x] restituisce il segno della derivata prima della funzione f."

(* Derivata Seconda *)
CalcoloDerivataSeconda::usage =
	"CalcoloDerivataSeconda[f, x] calcola la derivata seconda della funzione f."

SegnoDerivataSeconda::usage =
	"SegnoDerivataSeconda[f, x] restituisce il segno della derivata seconda della funzione f."

(* Grafico *)
GraficoFunzione::usage =
	"GraficoFunzione[f, x] disegna il grafico della funzione f."

Begin["`Private`"]

(* Dominio *)
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

SetAttributes[CalcolaContenutiDenominatoreDaControllare,HoldFirst];
CalcolaContenutiDenominatoreDaControllare[risultatoControlloDenominatore_,listaContenutiDenominatoreDaControllare_]:=(
	risultatoControlloDenominatore=Union[risultatoControlloDenominatore,Reduce[#<0,x,Reals]&/@listaContenutiDenominatoreDaControllare];
	risultatoControlloDenominatore=Union[risultatoControlloDenominatore,Reduce[#>0,x,Reals]&/@listaContenutiDenominatoreDaControllare];
);

CalcolaDominio[f_,x_]:=(
	fDivisa= DividiFunzioneInListaDiOperandi[f[x]];
	Print["I contenuti sotto radice pari "];
	listaContenutiRadiciDaControllare={};
	CalcolaContenutiRadiciDaControllare[listaContenutiRadiciDaControllare,#]&/@fDivisa;
	Print[listaContenutiRadiciDaControllare];
	Print[" devono essere maggiori di zero: "];
	risultatoControlloRadici={};
	risultatoControlloRadici=Reduce[#>0,x,Reals]&/@listaContenutiRadiciDaControllare;
	DeleteCases[risultatoControlloRadici,True];
	Print[risultatoControlloRadici];

	Print["I contenuti sotto logaritmi "];
	listaContenutiLogaritmoDaControllare ={};
	CalcolaContenutiLogaritmoDaControllare[listaContenutiLogaritmoDaControllare,#]&/@fDivisa;
	Print[listaContenutiLogaritmoDaControllare];
	Print[" devono essere maggiori di zero: "];
	risultatoControlloLogaritmo={};
	risultatoControlloLogaritmo=Reduce[#>0,x,Reals]&/@listaContenutiLogaritmoDaControllare;
	DeleteCases[risultatoControlloLogaritmo,True];
	Print[risultatoControlloLogaritmo];

	listaContenutiDenominatoreDaControllare = {};
	risultatoControlloDenominatore={};
	Print["Il denominatore: "];
	listaContenutiDenominatoreDaControllare = Append[listaContenutiDenominatoreDaControllare,Denominator[f[x]]];
	Print[listaContenutiDenominatoreDaControllare];
	Print[" deve essere diverso di zero, quindi calcolo i valori per cui il denominatore \[EGrave] minore di zero e maggiore di zero e risulta: "];
	CalcolaContenutiDenominatoreDaControllare[risultatoControlloDenominatore,listaContenutiDenominatoreDaControllare];
	DeleteCases[risultatoControlloDenominatore,True];
	Print[risultatoControlloDenominatore];

	Print["quindi facendo l'intersezione tra questi domini, il dominio \[EGrave]: "];
	dominio = FunctionDomain[f[x],x];
	Print[dominio];
);

(* Pari e dispari *)
ControlloPariDispari[f_, x_]:= (
  risultatoPari = FullSimplify[ForAll[x, f[x] == f[-x]]];
  risultatoDispari = FullSimplify[ForAll[x, -f[x] == f[-x]]];
  If[risultatoPari, Print["La funzione \[EGrave] pari."], 
   Print["La funzione non \[EGrave] pari."],
	Print["La funzione non \[EGrave] pari."]];
  If[risultatoDispari, Print["La funzione \[EGrave] dispari."], 
   Print["La funzione non \[EGrave] dispari."],
	Print["La funzione non \[EGrave] dispari."]];
); 

(* Intersezione con gli assi *)
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

(* Segno funzione *)
SegnoFunzione[f_, x_]:= (
	valore = Reduce[f[x] > 0, x, Reals];
	If[valore===False, Print["La funzione \[EGrave] negativa."],
	Print["La funzione \[EGrave] positiva per ", valore]];
); 

(* Calcola Limiti *)
ControlloSeLista[var_]:=(
	If[ListQ[var],var,{var}]
);

SetAttributes[CreazioneListaDominio,HoldFirst];
CreazioneListaDominio[listaDominio_,dominio_]:=(listaDominio=Union[listaDominio,ControlloSeLista[Select[dominio,Head[#]==Greater&]]];
	listaDominio=Union[listaDominio,ControlloSeLista[Select[dominio,Head[#]==GreaterEqual&]]];
	listaDominio=Union[listaDominio,ControlloSeLista[Select[dominio,Head[#]==Inequality&]]];
	listaDominio=Union[listaDominio,ControlloSeLista[Select[dominio,Head[#]==Less&]]];
	listaDominio=Union[listaDominio,ControlloSeLista[Select[dominio,Head[#]==LessEqual&]]];
	listaDominio = DeleteCases[listaDominio,False];
);

SetAttributes[CalcolaEstremiInferiori,HoldFirst];
CalcolaEstremiInferiori[estremiInferiori_,dominio_]:=(estremiInferiori=Union[estremiInferiori,#[[2]]&/@Select[dominio,Head[#]==Greater&]];
	estremiInferiori=Union[estremiInferiori,#[[2]]&/@Select[dominio,Head[#]==GreaterEqual&]];
	estremiInferiori=Union[estremiInferiori,If[#[[2]]===Less,#[[1]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiInferiori=Union[estremiInferiori,If[#[[2]]===LessEqual,#[[1]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiInferiori=Union[estremiInferiori,If[#[[2]]===Greater,#[[5]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiInferiori=Union[estremiInferiori,If[#[[2]]===GreaterEqual,#[[5]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiInferiori=DeleteCases[estremiInferiori,Null];
);

SetAttributes[CalcolaEstremiSuperiori,HoldFirst];
CalcolaEstremiSuperiori[estremiSuperiori_,dominio_]:=(estremiSuperiori=Union[estremiSuperiori,#[[2]]&/@Select[dominio,Head[#]==Less&]];
	estremiSuperiori=Union[estremiSuperiori,#[[2]]&/@Select[dominio,Head[#]==LessEqual&]];
	estremiSuperiori=Union[estremiSuperiori,If[#[[2]]===Less,#[[5]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiSuperiori=Union[estremiSuperiori,If[#[[2]]===LessEqual,#[[5]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiSuperiori=Union[estremiSuperiori,If[#[[2]]===Greater,#[[1]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiSuperiori=Union[estremiSuperiori,If[#[[2]]===GreaterEqual,#[[1]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiSuperiori=DeleteCases[estremiSuperiori,Null];
);

CalcolaLimiti[f_,x_]:=(
	dominio=FunctionDomain[f[x],x];
	dominio=If[Head[dominio]===Or,dominio,{dominio}];

	listaDominio={};
	CreazioneListaDominio[listaDominio,dominio];
	listaDominio;

	Print["Gli estremi inferiori per il calcolo del limite sono: "];
	estremiInferiori={};
	CalcolaEstremiInferiori[estremiInferiori,listaDominio];
	Print[estremiInferiori];

	Print["e gli estremi superiori sono: "];
	estremiSuperiori = {};
	CalcolaEstremiSuperiori[estremiSuperiori,listaDominio];
	Print[estremiSuperiori];

	calcolaInfinito=If[Max[estremiInferiori]>= Max[estremiSuperiori],True,False];
	calcolaMenoInfinito=If[Min[estremiInferiori]>= Min[estremiSuperiori],True,False];
	If[calcolaMenoInfinito,(Print["Il limite per x->",-\[Infinity]," nell'intorno destro = ",Limit[f[x],x->-Infinity],If[NumberQ[Limit[f[x],x->-Infinity]]," quindi \[EGrave] un asintoto orizzontale.", " quindi \[EGrave] un asintoto verticale."]])]
	If[calcolaInfinito,(Print["Il limite per x->",+\[Infinity]," nell'intorno sinistro = ",Limit[f[x],x->Infinity],If[NumberQ[Limit[f[x],x->Infinity]]," quindi \[EGrave] un asintoto orizzontale.", " quindi \[EGrave] un asintoto verticale."]])]
	Print["Il limite per x->",# ," nell'intorno destro = ",Limit[f[x],x->#,Direction->1],If[NumberQ[Limit[f[x],x->#,Direction->1]]," quindi \[EGrave] un asintoto orizzontale.", " quindi \[EGrave] un asintoto verticale."]]&/@estremiInferiori;
	Print["Il limite per x->",# ," nell'intorno sinistro = ",Limit[f[x],x->#,Direction->-1],If[NumberQ[Limit[f[x],x->#,Direction->-1]]," quindi \[EGrave] un asintoto orizzontale.", " quindi \[EGrave] un asintoto verticale."]]&/@estremiSuperiori;
);

(* Derivata Prima *)
StampaSoluzioni[soluzioni_] := (
	Print["Le soluzioni della derivata prima sono:"];
	Print[Column[soluzioni]];
)

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
	soluzioni = NSolve[num == 0, x, Reals];
	If[Length[soluzioni] == 0, Print["Non ci sono soluzioni per la derivata prima."],
		StampaSoluzioni[soluzioni]];
	intervalliPositivi = N@Reduce[d > 0, x, Reals];
	intervalliNegativi = N@Reduce[d < 0, x, Reals];
	If[StringCount[ToString[intervalliPositivi], "False"] > 0 || 
      StringCount[ToString[intervalliNegativi], "False"] > 0, 
      StampaMonotonia[intervalliPositivi, intervalliNegativi], 
      StampaIntervalli[intervalliPositivi, intervalliNegativi]];
	Print["Il grafico della derivata prima \[EGrave] il seguente:"];
	Plot[d, {x, -5, 5}]
)

(* Derivata Seconda *)
StampaConcavoConvessoNoSoluzioni[d2_] := If[d2 > 0, Print["La funzione \[EGrave] convessa"],
	Print["La funzione \[EGrave] concava"]];

GraficoDerivataSeconda[d_, x_, limite0_, limite1_] := (
	Print["Il grafico della derivata seconda \[EGrave] il seguente:"];
	Plot[d, {x, -5,+5}]
);

StampaSoluzioniDerivataSeconda[soluzioni_] := (
	Print["Le soluzioni della derivata seconda sono:"];
	Print[Column[soluzioni]];
)

StampaConcavoConvesso[intervalliConvessa_, intervalliConcava_] :=
  If[Length[intervalliConvessa] > 0, 
   Print["La funzione \[EGrave] convessa."], 
   Print["La funzione \[EGrave] concava"]];

StampaIntervalliDerivataSeconda[intervalliConvessa_, intervalliConcava_]:=(
	Print["Gli intervalli in cui la derivata seconda \[EGrave] positiva e dunque la funzione \[EGrave] convessa sono:"];
	Print[intervalliConvessa];
	Print["Gli intervalli in cui la derivata seconda \[EGrave] negativa e dunque la funzione \[EGrave] concava sono:"];
	Print[intervalliConcava];

);

CalcoloDerivataSeconda[f_, x_]:= D[D[f[x], x], x];

SegnoDerivataSeconda[f_, x_] := (
    d2 = CalcoloDerivataSeconda[f, x];
	Print["La derivata seconda \[EGrave]"];
	Print[d2];
	num = Numerator[d2];
	soluzioni = NSolve[num == 0, x, Reals];
	If[Length[soluzioni] == 0, Print["Non ci sono soluzioni per la derivata prima."],
		StampaSoluzioniDerivataSeconda[soluzioni]];
	intervalliConvessa = N@Reduce[d2 > 0, x, Reals];
	intervalliConcava = N@Reduce[d2 < 0, x, Reals];
	If[Length[intervalliConvessa] == 0 || Length[intervalliConcava] == 0,
		StampaConcavoConvessoNoSoluzioni[d2], StampaIntervalliDerivataSeconda[intervalliConvessa, intervalliConcava]];
	(*If[StringCount[ToString[intervalliConvessa], "False"] > 0 || 
  	StringCount[ToString[intervalliConcava], "False"] > 0,
  	StampaConcavoConvesso[intervalliConvessa, intervalliConcava],
	  StampaIntervalli[intervalliConvessa, intervalliConcava]];*)
	Print["Il grafico della derivata seconda \[EGrave] il seguente:"];
	Plot[d2, {x, -5, 5}]
)

(* Grafico *)
GraficoFunzione[f_, x_]:=(
	Print["Il grafico della funzione \[EGrave]: "];
	dominio=FunctionDomain[f[x],x];
	dominio=If[Head[dominio]===Or,dominio,{dominio}];

	listaDominio={};
	CreazioneListaDominio[listaDominio,dominio];

	estremiInferiori={};
	CalcolaEstremiInferiori[estremiInferiori,listaDominio];

	estremiSuperiori = {};
	CalcolaEstremiSuperiori[estremiSuperiori,listaDominio];

	calcolaInfinito=If[Max[estremiInferiori]>= Max[estremiSuperiori],True,False];
	calcolaMenoInfinito=If[Min[estremiInferiori]>= Min[estremiSuperiori],True,False];
	
	estremoInferiore=Min[estremiInferiori];
	estremoSuperiore=Max[estremiSuperiori];
	If[calcolaInfinito, estremoSuperiore=10];
	If[calcolaMenoInfinito, estremoInferiore=-10];

	Plot[f[x],{x, estremoInferiore, estremoSuperiore}]); 

End[]
EndPackage[]
