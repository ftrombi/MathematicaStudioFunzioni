(* ::Package:: *)

(* :Title: StudioFunzioni *)

(* :Context: StudioFunzioni` *)

(* :Author: Sebastian Davrieux e Francesco Trombi
	Informatica Magistrale *)

(* :Summary:
   Insieme di funzioni per il supporto allo studio di funzioni nelle scuole superiori.
 *)

(* :Copyright: Davrieux e Trombi, Matematica Computazionale, AA 2015/16, Mathematica v10.0.0.0 *)

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

(* Funzione che verifica se il parametro x \[EGrave] razionale. *)
VerificaRazionale[x_]:=If[Simplify[x\[Element]Rationals],True,False,Null];

(* Funzione che estrae il radicando del parametro x solo se contenuto in una radice pari. *)
PrendiContenutoRadice[x_]:=If[Head[x] ==Power,
	If[VerificaRazionale[Part[x,2]],
		If[EvenQ[Denominator[Part[x,2]]],Part[x,1],Null,Null], 
		Null, 
		Null], 
	Null,
	Null];

(* Funzione che estrae il contenuto del logaritmo del parametro x. *)
PrendiContenutoLogaritmo[x_]:=If[Head[x] ==Log,Part[x,1],Null,Null];

(* Funzione che estrae il secondo contenuto del parametro x se l'head del parametro \[EGrave] Times[] *)
PrendiSecondoContenutoSeHeadUgualeTimes[x_]:=If[Head[x] ==Times,Part[x,2],x,x];

(* Funzione che crea una lista di operandi estratti dal parametro x, ignorando il denominatore. *)
DividiFunzioneInListaDiOperandi[x_] :=If[Denominator[x]===1,
	List @@ Numerator[x],
	Join[List @@ Denominator[x], List@@Numerator[x]]];

(* SetAttributes[] consente il passaggio per riferimento del primo parametro della funzione CalcolaContenutiRadiciDaControllare[]. *)
SetAttributes[CalcolaContenutiRadiciDaControllare,HoldFirst];
(* Funzione che genera una lista con tutti i radicandi del parametro x. *)
CalcolaContenutiRadiciDaControllare[listaContenutiRadiciDaControllare_,x_]:=If[PrendiContenutoRadice[PrendiSecondoContenutoSeHeadUgualeTimes[x]] ===Null,
	Null,
	listaContenutiRadiciDaControllare=Append[listaContenutiRadiciDaControllare,PrendiContenutoRadice[PrendiSecondoContenutoSeHeadUgualeTimes[x]]]];

(* SetAttributes[] consente il passaggio per riferimento del primo parametro della funzione CalcolaContenutiLogaritmoDaControllare[]. *)
SetAttributes[CalcolaContenutiLogaritmoDaControllare,HoldFirst];
(* Funzione che genera una lista con tutti i contenuti dei logaritmi del parametro x. *)
CalcolaContenutiLogaritmoDaControllare[listaContenutiLogaritmoDaControllare_,x_]:=If[PrendiContenutoLogaritmo[PrendiSecondoContenutoSeHeadUgualeTimes[x]] ===Null,
	Null,
	listaContenutiLogaritmoDaControllare=Append[listaContenutiLogaritmoDaControllare,PrendiContenutoLogaritmo[PrendiSecondoContenutoSeHeadUgualeTimes[x]]]];

(* SetAttributes[] consente il passaggio per riferimento del primo parametro della funzione CalcolaContenutiDenominatoreDaControllare[]. *)
SetAttributes[CalcolaContenutiDenominatoreDaControllare,HoldFirst];
(* Funzione che genera una lista con tutti i contenuti del denominatore del parametro x. *)
CalcolaContenutiDenominatoreDaControllare[risultatoControlloDenominatore_,listaContenutiDenominatoreDaControllare_]:=(
	risultatoControlloDenominatore=Union[risultatoControlloDenominatore,Reduce[#<0,x,Reals]&/@listaContenutiDenominatoreDaControllare];
	risultatoControlloDenominatore=Union[risultatoControlloDenominatore,Reduce[#>0,x,Reals]&/@listaContenutiDenominatoreDaControllare];
);

(* Funzione che:
	- divide la funzione in una lista di operandi;
	- cerca i contenuti delle radici, dei logaritmi e del denominatore all'interno di essa;
	- calcola il dominio utilizzando queste liste;
	- per sicurezza, il dominio mostrato all'utente viene calcolato dalla funzione nativa FunctionDomain[].
*)
CalcolaDominio[f_,x_]:=(
	fDivisa= DividiFunzioneInListaDiOperandi[f[x]];
	listaContenutiRadiciDaControllare={};
	CalcolaContenutiRadiciDaControllare[listaContenutiRadiciDaControllare,#]&/@fDivisa;
	If[Length[listaContenutiRadiciDaControllare]==0, Print["Non ci sono radici ad indice pari."], 
		(Print["I radicandi sono: "];
		Print[listaContenutiRadiciDaControllare];
		Print["Sono maggiori di zero?"];
		risultatoControlloRadici={};
		risultatoControlloRadici=Reduce[#>0,x,Reals]&/@listaContenutiRadiciDaControllare;
		risultatoControlloRadici /. {True -> "vero"};
		risultatoControlloRadici /. {False -> "falso"};
		Print[risultatoControlloRadici])];
	
	listaContenutiLogaritmoDaControllare ={};
	CalcolaContenutiLogaritmoDaControllare[listaContenutiLogaritmoDaControllare,#]&/@fDivisa;
	If[Length[listaContenutiLogaritmoDaControllare]==0, Print["Non ci sono logaritmi da controllare"],	
		(Print["I conenuti dei logaritmi sono:"];
		Print[listaContenutiLogaritmoDaControllare];
		Print["Sono maggiori di zero?"];
		risultatoControlloLogaritmo={};
		risultatoControlloLogaritmo=Reduce[#>0,x,Reals]&/@listaContenutiLogaritmoDaControllare;
		risultatoControlloLogaritmo /. {True -> "vero"};
		risultatoControlloLogaritmo /. {False -> "falso"};
		Print[risultatoControlloLogaritmo])];

	listaContenutiDenominatoreDaControllare = {};
	risultatoControlloDenominatore={};
	Print["Il denominatore: "];
	listaContenutiDenominatoreDaControllare = Append[listaContenutiDenominatoreDaControllare,Denominator[f[x]]];
	Print[listaContenutiDenominatoreDaControllare];
	Print["Deve essere diverso di zero, quindi calcolo i valori per cui il denominatore \[EGrave] minore di zero e maggiore di zero e risulta: "];
	CalcolaContenutiDenominatoreDaControllare[risultatoControlloDenominatore,listaContenutiDenominatoreDaControllare];
	DeleteCases[risultatoControlloDenominatore,True];
	Print[risultatoControlloDenominatore];

	Print["Quindi facendo l'intersezione tra questi domini, il dominio \[EGrave]: "];
	dominio=FunctionDomain[f[x],x];
	Print[dominio];
);

(* Pari e dispari *)

(* Funzione che effettua il controllo della parit\[AGrave] e della disparit\[AGrave] di una funzione. *)
ControlloPariDispari[f_, x_]:=(
	fMenoX = f[x] /. {x->-x};
    risultatoPari = FullSimplify[ForAll[x,f[x] == fMenoX]];
    risultatoDispari = FullSimplify[ForAll[x,-f[x] == fMenoX]];
    If[risultatoPari, Print["La funzione \[EGrave] pari."], 
      Print["La funzione non \[EGrave] pari."],
	  Print["La funzione non \[EGrave] pari."]];
    If[risultatoDispari, Print["La funzione \[EGrave] dispari."], 
      Print["La funzione non \[EGrave] dispari."],
	  Print["La funzione non \[EGrave] dispari."]];
); 

(* Intersezione con gli assi *)

(* Funzione che controlla la presenza di intersezione con gli assi, stampandone i risultati in caso di presenza. *)
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

(* Funzione che valuta gli intervalli in cui la funzione \[EGrave] positiva. *)
SegnoFunzione[f_, x_]:= (
	valore = Reduce[f[x] > 0, x, Reals];
	If[valore===False, Print["La funzione \[EGrave] negativa."],
	Print["La funzione \[EGrave] positiva per ", valore]];
); 

(* Calcola Limiti *)

(* Funzione che controlla se il parametro var \[EGrave] una lista. *)
ControlloSeLista[var_]:=(
	If[ListQ[var],var,{var}]
);

(* SetAttributes[] consente il passaggio per riferimento del primo parametro della funzione CreazioneListaDominio[]. *)
SetAttributes[CreazioneListaDominio,HoldFirst];
(* Funzione che crea una lista contenente gli intervalli del dominio di definizione della funzione, definito utilizzando la FunctionDomain[]. *)
CreazioneListaDominio[listaDominio_,dominio_]:=(listaDominio=Union[listaDominio,ControlloSeLista[Select[dominio,Head[#]==Greater&]]];
	listaDominio=Union[listaDominio,ControlloSeLista[Select[dominio,Head[#]==GreaterEqual&]]];
	listaDominio=Union[listaDominio,ControlloSeLista[Select[dominio,Head[#]==Inequality&]]];
	listaDominio=Union[listaDominio,ControlloSeLista[Select[dominio,Head[#]==Less&]]];
	listaDominio=Union[listaDominio,ControlloSeLista[Select[dominio,Head[#]==LessEqual&]]];
	listaDominio = DeleteCases[listaDominio,False];
);

(* SetAttributes[] consente il passaggio per riferimento del primo parametro della funzione CalcolaEstremiInferiori[]. *)
SetAttributes[CalcolaEstremiInferiori,HoldFirst];
(* Funzione che crea una lista contenente gli estremi inferiori del dominio di definizione della funzione formattato dalla funzione CreazioneListaDominio[]. *)
CalcolaEstremiInferiori[estremiInferiori_,dominio_]:=(estremiInferiori=Union[estremiInferiori,#[[2]]&/@Select[dominio,Head[#]==Greater&]];
	estremiInferiori=Union[estremiInferiori,#[[2]]&/@Select[dominio,Head[#]==GreaterEqual&]];
	estremiInferiori=Union[estremiInferiori,If[#[[2]]===Less,#[[1]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiInferiori=Union[estremiInferiori,If[#[[2]]===LessEqual,#[[1]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiInferiori=Union[estremiInferiori,If[#[[2]]===Greater,#[[5]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiInferiori=Union[estremiInferiori,If[#[[2]]===GreaterEqual,#[[5]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiInferiori=DeleteCases[estremiInferiori,Null];
);

(* SetAttributes[] consente il passaggio per riferimento del primo parametro della funzione CalcolaEstremiSuperiori[]. *)
SetAttributes[CalcolaEstremiSuperiori,HoldFirst];
(* Funzione che crea una lista contenente gli estremi superiori del dominio di definizione della funzione formattato dalla funzione CreazioneListaDominio[]. *)
CalcolaEstremiSuperiori[estremiSuperiori_,dominio_]:=(estremiSuperiori=Union[estremiSuperiori,#[[2]]&/@Select[dominio,Head[#]==Less&]];
	estremiSuperiori=Union[estremiSuperiori,#[[2]]&/@Select[dominio,Head[#]==LessEqual&]];
	estremiSuperiori=Union[estremiSuperiori,If[#[[2]]===Less,#[[5]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiSuperiori=Union[estremiSuperiori,If[#[[2]]===LessEqual,#[[5]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiSuperiori=Union[estremiSuperiori,If[#[[2]]===Greater,#[[1]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiSuperiori=Union[estremiSuperiori,If[#[[2]]===GreaterEqual,#[[1]]]&/@Select[dominio,Head[#]==Inequality&]];
	estremiSuperiori=DeleteCases[estremiSuperiori,Null];
);

(* Funzione che:
	- calcola il dominio della funzione presa in input;
	- formatta gli intervalli inserendoli in una lista;
	- calcola gli estremi inferiori e superiori;
	- calcola i limiti su tali estremi;
	- controlla il risultato per identificare asintoti.
*)
CalcolaLimiti[f_,x_]:=(
	dominio=FunctionDomain[f[x],x];
	dominio=If[Head[dominio]===Or,dominio,{dominio}];

	listaDominio={};
	CreazioneListaDominio[listaDominio,dominio];
	listaDominio;

	Print["Gli estremi inferiori per il calcolo del limite sono: "];
	estremiInferiori={};
	CalcolaEstremiInferiori[estremiInferiori,listaDominio];
	If[Length[estremiInferiori]==0,(calcolaMenoInfinito=True; Print[-\[Infinity]]),Print[estremiInferiori]];

	Print["e gli estremi superiori sono: "];
	estremiSuperiori = {};
	CalcolaEstremiSuperiori[estremiSuperiori,listaDominio];
	If[Length[estremiSuperiori]==0,(calcolaInfinito=True; Print[+\[Infinity]]),Print[estremiSuperiori]];

	calcolaInfinito=If[Max[estremiInferiori]>= Max[estremiSuperiori],True,False];
	calcolaMenoInfinito=If[Min[estremiInferiori]>= Min[estremiSuperiori],True,False];
	If[calcolaMenoInfinito===True,
		(Print["Il limite per x->",-\[Infinity]," nell'intorno destro = ",Limit[f[x],x->-Infinity],
			If[NumberQ[Limit[f[x],x->-Infinity]]," quindi \[EGrave] un asintoto orizzontale.", " quindi \[EGrave] un asintoto verticale."]])];
	If[calcolaInfinito===True,
		(Print["Il limite per x->",+\[Infinity]," nell'intorno sinistro = ",Limit[f[x],x->Infinity],
			If[NumberQ[Limit[f[x],x->Infinity]]," quindi \[EGrave] un asintoto orizzontale.", " quindi \[EGrave] un asintoto verticale."]])];
	Print["Il limite per x->",# ," nell'intorno destro = ",Limit[f[x],x->#,Direction->-1],If[NumberQ[Limit[f[x],x->#,Direction->-1]]," quindi \[EGrave] un asintoto orizzontale.", " quindi \[EGrave] un asintoto verticale."]]&/@estremiInferiori;
	Print["Il limite per x->",# ," nell'intorno sinistro = ",Limit[f[x],x->#,Direction->1],If[NumberQ[Limit[f[x],x->#,Direction->1]]," quindi \[EGrave] un asintoto orizzontale.", " quindi \[EGrave] un asintoto verticale."]]&/@estremiSuperiori;
);

(* Derivata Prima *)
(* Funzione che permette di stampare in maniera formattata le soluzioni della derivata prima. *)
StampaSoluzioni[soluzioni_] := (
	Print["Le soluzioni della derivata prima sono:"];
	Print[Column[soluzioni]];
)

(* Funzione che permette di stampare in maniera formattata la monotonia della funzione. *)
StampaMonotonia[intervalliPositivi_, intervalliNegativi_] :=
  If[Length[intervalliPositivi] > 0, 
   Print["La funzione \[EGrave] monotona crescente"], 
   Print["La funzione \[EGrave] monotona decrescente"]];

(* Funzione che permette di stampare in maniera formattata gli intervalli di monotonia della funzione primitiva. *)
StampaIntervalli[listaIntervalliPositivi_, listaIntervalliNegativi_]:=(
	Print["Gli intervalli in cui la derivata \[EGrave] positiva sono:"];
	Print[listaIntervalliPositivi];
	Print["Gli intervalli in cui la derivata \[EGrave] negativa sono:"];
	Print[listaIntervalliNegativi];

);

(* Funzione che calcola la derivata prima della funzione. *)
CalcoloDerivataPrima[f_, x_]:= Simplify[D[f[x], x]];

(* Funzione che:
	- calcola la derivata prima della funzione;
	- stampa la derivata prima;
	- controlla la presenza di soluzioni della derivata prima;
	- determina gli intervalli di monotonia;
	- stampa il grafico della derivata prima, utilizzando gli estremi del dominio della derivata prima per limitarlo.
*)
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

	Plot[d, {x, estremoInferiore, estremoSuperiore}]
)

(* Derivata Seconda *)

(* Funzione che stampa in maniera formattata la convessit\[AGrave] e la concavit\[AGrave] di una funzione, se la derivata seconda \[EGrave] priva di soluzioni. *)
StampaConcavoConvessoNoSoluzioni[d2_] := If[d2 > 0, Print["La funzione \[EGrave] convessa"],
	Print["La funzione \[EGrave] concava"]];

(* Funzione che stampa il grafico della derivata seconda. *)
GraficoDerivataSeconda[d_, x_, limite0_, limite1_] := (
	Print["Il grafico della derivata seconda \[EGrave] il seguente:"];
	Plot[d, {x, -5,+5}]
);

(* Funzione che permette di stampare in maniera formattata le soluzioni della derivata seconda. *)
StampaSoluzioniDerivataSeconda[soluzioni_] := (
	Print["Le soluzioni della derivata seconda sono:"];
	Print[Column[soluzioni]];
)

(* Funzione che stampa in maniera formattata la convessit\[AGrave] e la concavit\[AGrave] di una funzione. *)
StampaConcavoConvesso[intervalliConvessa_, intervalliConcava_] :=
  If[Length[intervalliConvessa] > 0, 
   Print["La funzione \[EGrave] convessa."], 
   Print["La funzione \[EGrave] concava"]];

(* Funzione che stampa in maniera formattata gli intervalli di convessit\[AGrave] e concavit\[AGrave] della funzione primitiva. *)
StampaIntervalliDerivataSeconda[intervalliConvessa_, intervalliConcava_]:=(
	Print["Gli intervalli in cui la derivata seconda \[EGrave] positiva e dunque la funzione \[EGrave] convessa sono:"];
	Print[intervalliConvessa];
	Print["Gli intervalli in cui la derivata seconda \[EGrave] negativa e dunque la funzione \[EGrave] concava sono:"];
	Print[intervalliConcava];

);

(* Funzione che permette di calcolare la derivata seconda della funzione. *)
CalcoloDerivataSeconda[f_, x_]:= D[D[f[x], x], x];

(* Funzione che:
	- calcola la derivata seconda della funzione;
	- stampa la derivata seconda;
	- controlla la presenza di soluzioni della derivata seconda;
	- determina gli intervalli di convessit\[AGrave] e concavit\[AGrave];
	- stampa il grafico della derivata seconda, utilizzando gli estremi del dominio della derivata seconda per limitarlo.
*)
SegnoDerivataSeconda[f_, x_] := (
    d2 = CalcoloDerivataSeconda[f, x];
	Print["La derivata seconda \[EGrave]"];
	Print[d2];
	num = Numerator[d2];
	soluzioni = NSolve[num == 0, x, Reals];
	If[Length[soluzioni] == 0, Print["Non ci sono soluzioni per la derivata seconda."],
		StampaSoluzioniDerivataSeconda[soluzioni]];
	intervalliConvessa = N@Reduce[d2 > 0, x, Reals];
	intervalliConcava = N@Reduce[d2 < 0, x, Reals];
	If[Length[intervalliConvessa] == 0 || Length[intervalliConcava] == 0,
		StampaConcavoConvessoNoSoluzioni[d2], StampaIntervalliDerivataSeconda[intervalliConvessa, intervalliConcava]];
	Print["Il grafico della derivata seconda \[EGrave] il seguente:"];

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


	Plot[d2, {x, estremoInferiore, estremoSuperiore}]
)

(* Grafico *)

(* Funzione che stampa il grafico della funzione, utilizzando gli estremi del dominio per limitarlo. *)
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
Protect[StudioFunzioni];
EndPackage[]
