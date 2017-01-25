%%% coding: utf-8
%%% @author CHRIS
%%% @doc Belegaufgabe 3
-module(bel3_2).
-compile(export_all). % alle Methode "public" machen


%% ============================================================================
%% ZUM KOPIEREN c(bel3_2). bel3_2:m3(4). 
%% ============================================================================

%% ============================================================================
%% HILFSFUNKTIONEN
%% ============================================================================

%% 
%% braucht man vllt. zum Debuggen
%%
print(P) -> io:write(P), io:fwrite("~n"), true.


%% 
%% Prozess Nachrichten ausdrucken
%%
pDebug(P, E, N, X) when is_atom(X) -> 
  case X of
    pstart -> io:format("~p >>> ~p aus Node: ~p~n", [E, P, N]);
	pende -> io:format("~p <<< ~p Node war: ~p~n", [E, P, N]);
	fertig -> io:format("Loopgather ~p von Node: ~p fertig! Länge:~p~n", [P, N, E])
  end.


%% 
%% um das Test-Kommando zu vereinfachen
%%
t() -> eunit:test(bel3_2).


%% 
%% zum Zeit messen einer Funktion f 
%%
tshit(F) -> Start = os:timestamp(),
  F(), io:format("Dauer: ~f seconds~n", [(timer:now_diff(os:timestamp(), Start)) / 1000000]).
 

%% 
%% um das ganze schneller in der ErlangShell zu testen, hier als Template
%% Aufruf: mom() -> bel3_2:tshit(fun() -> bel3_2:FUNKTION ( ARGS ) end).
%%
mom1() -> bel3_2:tshit(fun() -> bel3_2:calcSquares([15,10,3,6], 4, 34) end). % 0.4 secs


%% 
%% Zu faul das immer hinzuschreiben...
%%
m1(X) -> bel3_2:magicsquare(X). % ~10 mins
m2(X) -> bel3_2:distribMS(X, kerne()). % ~4-5 mins mit 4 Kerne
m2(X, Y) -> bel3_2:distribMS(X, Y).
m3(X) -> bel3_2:megaDistribMS(X). % 5 mins 


%% 
%% Wieviele Kerne haben wir zur Verfügung?
%%
kerne() -> erlang:system_info(logical_processors_available). % 4 bei mir


%% 
%% zur Berechnung der magischen Value aus Max (Seitenlänge)
%%
getValue(Max) -> trunc((math:pow(Max, 3) + Max) / 2).



%% ============================================================================
%% TYP-DEFINITIONEN
%% ============================================================================

%% 
%% nur um Platz zu sparen
%%
-type posint() :: non_neg_integer().


%% ============================================================================
%% Algorithmus fuer die verteilte Berechnung Magischer Quadrate
%% ============================================================================

%% ============================================================================
%% Teil 1 - Berechnung magischer Quadrate auf einem Rechner
%% ============================================================================

%%
%% Berechnet alle moeglichen Zeilen eines Magischen Quadrats
%% Aufruf: row(Max, Value, Elements) - z.B. row(3,15,lists:seq(1,15))
%% Max - Seitengroesse des Quadrats
%% Value - Wert der Summe der Zeile
%% Elements - Elemente aus denen ausgewaehlt werden soll
%%
%% spec ist die Funktionsdeklaration (Signatur); row/3 muss 2 Ints und
%% ne Intliste als Parameter haben und ne Liste von Intlisten zurückgeben
%%
%% ERKLÄRUNG: alles passiert in row/4
%% Max wurde doppelt genommen, damit man zählen kann
%% Listengenerator; Zif ist die Ziffer, NumListe ist die Intliste
%% Wir machen also jeweils Max Ints in eine Liste, wobei
%% die Summe der Max gleich Value sein muss und keine Doppelten
%% was die Guards machen:
%% Wenn Ziffer schon in der Nummernliste ist, abbrechen
%% Count ist ja 1 bis Max, d.h. zeigt an, wie vollständig die Nummernliste ist
%% Wenn Count nicht Max ist, steht ein FALSE vor dem ANDALSO, d.h.
%% wir kontrollieren den Ausdruck nach ANDALSO also gar nicht mehr,
%% das FALSE wird negiert und wir haben TRUE
%% Macht ja auch Sinn, die Zeile ist noch nicht fertig, da brauchen
%% wir ja noch nicht die Summe der Zeile prüfen
%% Wenn Count Max ist, steht TRUE vor dem ANDALSO, d.h.
%% jetzt wird auch der 2. Ausruck ausgewertet:
%% Die Zeile ist jetzt vollständig (wir haben Max Ziffern) und
%% die Summe muss jetzt Value sein, wenn ja:
%% erhalten wir NOT(TRUE AND FALSE) -> NOT(FALSE) -> TRUE
%% Macht ja Sinn, Zeile ist vollständig und Summe stimmt
%% Wenn die Summe nicht Value ist, erhalten wir:
%% NOT(TRUE AND TRUE) -> NOT(TRUE) -> FALSE
%%
-spec row(posint(), posint(), list(posint())) -> list(list(posint())).
row(Max, Value, Elements) ->  row(Max, Max, Value, Elements).

-spec row(posint(), posint(), posint(), list(posint())) -> list(list(posint())).
row(0, _, _, _) -> [[]];
row(Count, Max, Value, Elems) ->
  [[Zif | NumListe] || 
    NumListe <- row(Count - 1, Max, Value, Elems),   
    Zif <- Elems, not(lists:member(Zif, NumListe)),
    not((Count == Max) andalso (lists:sum([Zif | NumListe]) /= Value))].


%%
%% Funktion, die ermittelt, ob sich in zwei Listen doppelte Elemente befinden
%% spec ist die Funktionsdeklaration (Signatur); duplicate/2 muss zwei
%% Listen von Ints als Parameter haben und nen BOOLEAN zurückgeben
%%
%% ERKLÄRUNG: Die hier war vorgegeben zum erweitern;
%% erste Version war das hier:
%% duplicate(L1, L2) -> lists:any(fun(E) -> lists:member(E, L2) end, L1).
%%
%% war aber arschlangsam im Vergleich
%%
-spec duplicate(list(posint()), list(posint())) -> boolean().
duplicate(L1, L2 )-> duplicate(lists:sort(L1 ++ L2)).

-spec duplicate(list(posint())) -> boolean().
duplicate([]) -> false;
duplicate([E, E| _]) -> true;
duplicate([_| R]) -> duplicate(R).


%%
%% combineRows setzt eine beliebige Anzahl von Reihen, die vorab berechnet
%% werden, zusammen. Dabei wird ueberprueft, ob sich doppelte Elemente 
%% innerhalb der Reihen befinden. Aufruf: combineRows(Col, Max, Value, Elems)
%% Col - Anzahl der Reihen, die berechnet werden sollen
%% Max - Anzahl der Elemente pro Zeile
%% Value - Wert der Summe der Zeile
%% Elems - Elemente, aus denen gewaehlt werden soll
%%
%% spec ist die Funktionsdeklaration (Signatur); combineRows/3 muss 3 Ints
%% als Parameter haben und ne Liste von Integerlisten zurückgeben
%%
%% ERKLÄRUNG: Hilfsfunktion, damit magicsquare/2 funktioniert
%% Dort wird combineRows nämlich mit 3 Params aufgerufen...
%% Vorgegeben war combineRows aber mit 4 Params, wobei der 4. die ElementeListe ist
%% Bedeutet: Haben wir nur Col, max und Value, erstellen wir uns eben ne Elementeliste,
%% bestehend aus den Zahlen von 1 bis Max^2
%% macht ja auch Sinn: Ein Quadrat mit X Spalten/Zeilen hat X^2 Elemente
%%
-spec combineRows(posint(), posint(), posint()) -> list(list(posint())).
combineRows(Col, Max, Value) -> 
  combineRows(Col, Max, Value, lists:seq(1, Max * Max)).


%%
%% spec ist die Funktionsdeklaration (Signatur); combineRows/4 muss 3 Ints und
%% ne Intliste als Parameter haben und ne Liste von Integerlisten zurückgeben
%%
%% ERKLÄRUNG: Das ist die combineRows Funktion, die vorgegeben war als Rumpf
%% Die macht letztendlich nichts anderes, als combine/2 aufzurufen
%% und dort passiert dann der ganze Shit
%%
-spec combineRows(posint(), posint(), posint(), list(posint())) -> list(list(posint())).
combineRows(Col, Max, Value, Elems) -> combine(Col, row(Max, Value, Elems)). 


%%
%% spec ist die Funktionsdeklaration (Signatur); combine/2 muss 1 Int und ne
%% Liste von Intlisten als Parameter haben und ne Liste von Integerlisten 
%% zurückgeben
%%
%% ERKLÄRUNG: Hier passiert die ganze Masse:
%% Wenn Col 0 ist, gib leere Liste zurück (Abbruchbedingung)
%% Ansonsten haben wir nen Listengenerator: der erstellt eine Liste
%% jedes Element dieser Liste ist wiederum eine Liste
%% Es wird immer überprüft, ob doppelte enthalten sind
%% Sobald duplicate/2 true wird, bricht der Zweig ab
%%
-spec combine(posint(), list(list(posint()))) -> list(list(posint())).
combine(0, _) -> [[]];
combine(Col, Zeilen) -> 
  [CR ++ Z || 
    CR <- combine(Col - 1, Zeilen),  
    Z <- Zeilen, 
    not(duplicate(CR, Z)) ].


%%	
%% calcSquares berechnet aus einem Teilquadrat alle moeglichen gueltigen 
%% Quadrate, die sich bilden lassen; Aufruf: calcSquares(Part, Max, Value)
%% Part - Teilquadrat fuer das die Magischen Quadrate berechnet werden sollen
%% Max - Anzahl der Elemente pro Zeile/Spalte
%% Value - Wert der Summe einer Zeile
%%
%% spec ist die Funktionsdeklaration (Signatur); calcSquares/3 muss ne Liste
%% von Intlisten und 2 Ints als Parameter haben und ne Liste von Intlisten
%% zurückgeben
%%
%% ERKLÄRUNG: Als erstes analysieren wir das "Teilquadrat";
%% Mit length(Part) div Max ermitteln wir, wie viele Zeilen wir schon haben
%% z.B. 2 Zeilen eines 3x3 wäre eine Liste der Länge 6 -> 6/3 = 2 Zeilen
%% Das ziehen wir von der max Zeilenanzahl ab, und davon dann noch 1
%%
%% Wir berechnen damit dann alle möglichen Zeilen außer der letzten
%% Kommt also eine 3er Zeile rein, berechnen wir 1 weitere
%% Kommt eine 4er Zeile rein, berechnen wir 2 weitere
%%
%% "Moegliche" kombiniert dann die gegebenen und die berechneten
%% Bei einer 4er Zeile Input haben wir also jetzt alle möglichen
%% Kombinationen von den ersten 3 Zeilen eines magischen 4x4 Quadrates 
%%
%% Dann kombinieren wir jede 3 Zeilen Kombi mit allen möglichen letzten Zeilen
%% Die Abbruchbedingungen sind dabei alle per ANDALSO gekettet, damit
%% beim ersten FALSE sofort rausgegangen wird
%% Dadurch erreichen wir nen Geschwindigkeitsvorteil:
%% Wenn die Zeilensumme der letzten Zeile nicht passt, wäre es ja sinnlos
%% noch die Diagonalen etc zu checken, weil es eh schon falsch ist
%% Nur wenn ein Check TRUE war, machen wir noch einen, usw...
%% Wir machen also immer nur so viel, wie nötig
%%
%% Einfach alle restlichen Zeilen zu berechnen und immer ein volles Quadrat
%% auf Magie zu checken, dauert im Vergleich viiiiieel länger
%% und verbraucht so viel Speicherplatz, das mein Arbeitsspeicher immer 
%% aufgegeben hat :( 
%%
%% Der Vorteil ist hier, das für letzte Zeilen die Werte stark eingeschränkt 
%% werden können, da die jeweilige Spaltensumme ja bis auf einen Wert 
%% schon existiert, d.h. MagicNumber - bisherige Spaltensumme = gesuchter Wert
%%
-spec calcSquares(list(posint()), posint(), posint()) -> list(list(posint())).
calcSquares(Part, Max, Value) -> 
  Zeilen = combineRows(Max - length(Part) div Max - 1, Max, Value, lists:seq(1, Max * Max) -- Part),
  Moegliche = [Part ++ Y || Y <- Zeilen, not(duplicate(Y, Part))],
  [C ++ Letzte || C <- Moegliche, Letzte <- [calcLetzten(C, Max, Value)], 
	not(duplicate(Letzte, C)) 
	andalso length(Letzte) == Max 
	andalso checkLR(C ++ Letzte, Max, Value)
	andalso checkRL(C ++ Letzte, Max, Value) 
	].


%%
%% spec ist die Funktionsdeklaration (Signatur); checkLR/3 muss ne Liste
%% von Intlisten und 2 Ints als Parameter haben und nen BOOLEAN
%% zurückgeben
%%
%% ERKLÄRUNG: Hilfsfunktion zur Überprüfung auf Magie
%% Wir wenden eine Faltungsfunktion an:
%% Dia speichert die Summe der LR-Diagonalen des Quadrates Q
%% Dia ist also der Akkumulator von foldl und am Anfang 0
%% Die Startliste dafür ist [1,...,M], wobei M die Zeilenanzahl ist
%% die fun() addiert den Wert an der Stelle M * (Z - 1) + Z zum Akku hinzu
%% zum Schluss wird die LR-Diagonalensumme mit der magischen Zahl V verglichen
%% z.B. für ein 3x3 Quadrat [1,2,3,4,5,6,7,8,9]:
%% Die Form ist: 	[1,2,3   \
%%					 4,5,6    \
%%					 7,8,9]    \
%% Die Index für die LR-Diagonale sind also 1,5,9
%% Für ein 4x4 wären die Indexe: 1,6,11,16
%% Es lässt sich ne Formel entwickeln für die Indexe:
%% Spaltenanzahl * (momentane Zeile - 1) + momentane Zeile
%% siehe dazu extract/4
%% für 3x3 also: 3*(Zeile-1)+Zeile -> 3*0+1,3*1+2,3*2+3 -> 1,5,9
%% für 4x4: 4*(Zeile-1)+Zeile -> 4*0+1,4*1+2,4*2+3,4*3+4 -> 1,6,11,16
%% Es wird dann immer der Wert der Quadratliste an dieser Stelle rausgeholt
%% mit lists:nth() und zum Akku addiert (also zur Diagonalensumme)
%% wir falten von links nach rechts rüber und haben am Ende eine Summe,
%% die dann mit V verglichen wird
%%
-spec checkLR(list(list(posint())), posint(), posint()) -> boolean().
checkLR(Q, M, V) ->
  V == lists:foldl(fun(Z, Dia) -> 
    Dia + extract(Q, M, Z, Z) end, 0, lists:seq(1, M)).
  

%%
%% spec ist die Funktionsdeklaration (Signatur); checkRL/3 muss ne Liste
%% von Intlisten und 2 Ints als Parameter haben und nen BOOLEAN
%% zurückgeben
%%  
%% ERKLÄRUNG: Hilfsfunktion zur Überprüfung auf Magie
%% Wir wenden eine Faltungsfunktion an:
%% Dia speichert die Summe der RL-Diagonalen des Quadrates Q
%% Dia ist also der Akkumulator von foldl und am Anfang 0
%% Die Startliste dafür ist [1,...,M], wobei M die Zeilenanzahl ist
%% die fun() addiert den Wert an der Stelle (M-1) * Z + 1 zum Akku hinzu
%% zum Schluss wird die RL-Diagonalensumme mit der magischen Zahl V verglichen
%% und je nachchdem TRUE oder FALSE zurückgegeben
%% z.B. für ein 3x3 Quadrat [1,2,3,4,5,6,7,8,9]:
%% Die Form ist: 	[1,2,3     /
%%					 4,5,6    /
%%					 7,8,9]  /
%% Die Index für die RL-Diagonale sind also 3,5,7
%% Für ein 4x4 wären die Indexe: 4,7,10,13
%% Es lässt sich ne Formel entwickeln für die Indexe:
%% (Spaltenanzahl - 1) * momentane Zeile + 1
%% siehe dazu extract/4
%% für 3x3 also: (3-1)*Zeile+1 -> 2*1+1, 2*2+1, 2*3+1 -> 3,5,7
%% für 4x4: (4-1)*Zeile+1 -> 3*1+1, 3*2+1, 3*3+1, 3*4+1 -> 4,7,10,13
%% Es wird dann immer der Wert der Quadratliste an dieser Stelle rausgeholt
%% mit lists:nth() und zum Akku addiert (also zur Diagonalensumme)
%% wir falten von links nach rechts rüber und haben am Ende eine Summe,
%% die dann mit V verglichen wird
%%
-spec checkRL(list(list(posint())), posint(), posint()) -> boolean().
checkRL(Q, M, V) ->
  V == lists:foldl(fun(Z, Dia) -> 
    Dia + extract(Q, Z, M, 1) end, 0, lists:seq(1, M)).
  
  
%%
%% spec ist die Funktionsdeklaration (Signatur); extract/4 muss ne Liste
%% von Intlisten und 3 Ints als Parameter haben und nen Int 
%% zurückgeben
%%  
%% ERKLÄRUNG: Hilfsfunktion für den DiagonalenCheck
%% aus checkLR/3 und checkRL/3 zur Vereinfachung
%% M ist dabei das Quadrat als Liste von Intlisten
%% A,B,C berechnen den Index an der Stelle, die zur Diagonale gehört
%% für die LR-Diagonale ist die Formel: 
%% Spaltenanzahl * (momentane Zeile - 1) + momentane Zeile
%% Das lässt sich abstrahieren auf das Format A * (B - 1) + C
%% mit A = Spaltenanzahl, B und C = momentane Zeile
%% Und genauso rufen wir extract/4 in checkLR/3 auf
%% Selbiges für RL; die Formel ist:
%% (Spaltenanzahl - 1) * momentane Zeile + 1
%% Das lässt sich auch abstrahieren auf das Format A * (B - 1) + C
%% mit A = momentane Zeile, B = Spaltenanzahl und C einfach 1
%% (merke: wir haben die Faktoren vertauscht zur besseren Übersicht)
%%
-spec extract(list(list(posint())), posint(), posint(), posint()) -> posint().
extract(M, A, B, C) -> lists:nth(A * (B - 1) + C, M).


%%
%% spec ist die Funktionsdeklaration (Signatur); calcLetzten/3 muss ne Liste
%% von Intlisten und 2 Ints als Parameter haben und nen Intliste 
%% zurückgeben
%% 
%% ERKLÄRUNG: 
%% letzte Zeile, anhand von denen die schon da sind, berechnen
%% weil mit der Spaltensumme hat man ja ne Vorgabe,
%% was überhaupt nur übrig bleibt
%%
-spec calcLetzten(list(posint()), posint(), posint()) -> list(posint()).
calcLetzten(Part, N, Value) -> 
  [Value - Sum || 
    Spalte <- lists:seq(1, N), 
    Sum <- [calcSpaltenSum(Part, Spalte, N)], 
	Value - Sum >= 1
  ].


%%
%% spec ist die Funktionsdeklaration (Signatur); calcSpaltenSum/3 muss ne Liste
%% von Intlisten und 2 Ints als Parameter haben und nen Intliste 
%% zurückgeben
%%
%% TODO: in extract/4 auslagern
%% 
%% ERKLÄRUNG: Faltungsfunktion von links nach rechts
%% So wie beim Diagonalen checken, wir berechnen die Summe
%% der bisherigen Spalte, d.h. der entstehende Wert ist kleiner als MagicNumber
%%
-spec calcSpaltenSum(list(posint()), posint(), posint()) -> posint().
calcSpaltenSum(Part, Spal, N) ->
  lists:foldl(fun(E, Sum) -> 
    Sum + lists:nth(E * N + Spal, Part) end, 0, lists:seq(0, N - 2)). 
  

%%
%% combineSquares ermittelt aus allen Teilquadraten die gueltige Loesung
%% Aufruf: combineSquares(Parts, Max, Value)
%% Parts - Alle Teilquadrate
%% Max - Anzahl der Zeilen
%% Value - Wert der Summe einer Zeile
%%
%% spec ist die Funktionsdeklaration (Signatur); combineSquares/4 muss ne Liste
%% von Intlisten und 3 Ints als Parameter haben und ne Liste von Intlisten 
%% zurückgeben
%%
%% ERKLÄRUNG: Das hier ist für den Debugfall (siehe magicsquare/2)
%% War schon vorgegeben von der Grundstruktur her
%% Wenn eine leere Liste reingeht, kommt eine leere Liste rausnehmen
%% Ansonsten wird immer das erste Element genommen (ein Random-Quadrat)
%% Damit wird calcSquares/3 aufgerufen und das Result betrachtet:
%% Falls leer, einfach neu aufrufen mit dem nächsten Random-Quadrat
%% Ansonsten, eine DebugNachricht drucken...
%% Das Ergebnis bildet den Anfang der Ausgabeliste
%% Diese wird dann erweitert um das Ergebnis des Rests
%% Merke: Num wird erhöht... ist halt nichts weiter als nen Counter (Debug halt)
%%
-spec combineSquares(list(list(posint())), posint(), posint(), integer()) -> list(list((posint()))).
combineSquares([], _, _, _) -> [];
combineSquares([X| XS], Max, Value, Num) ->
  Res = calcSquares(X, Max, Value),
    case Res of 
    [] -> combineSquares(XS, Max, Value, Num);
    _ -> io:format("Erg Nummer~p:~p~n",[Num, Res]), 
	     Res ++ combineSquares(XS, Max, Value, Num + length(Res))
    end.

	
%%
%% spec ist die Funktionsdeklaration (Signatur); combineSquares/3 muss ne 
%% Liste von Intlisten und 2 Ints als Parameter haben und ne Liste von 
%% Intlisten zurückgeben
%%
%% ERKLÄRUNG: Das hier ist für den Normalfall (kein DebugModus) 
%% (siehe magicsquare/1)
%% War schon vorgegeben von der Grundstruktur her
%% Für jede Liste wird die calcSquares/3 aufgerufen
%% Bedeutet: Es geht eine Liste von Random-Quadraten hinein und es kommt
%% eine Liste mit Quadraten heraus, die magisch sind
%% Die Ergbenisliste wird um eine Ebene flachgezogen (flatmap)
%%
-spec combineSquares(list(list(posint())), posint(), posint()) -> list(list((posint()))).
combineSquares(Parts, Max, Value) ->
  lists:flatmap(fun(X) -> calcSquares(X, Max, Value) end, Parts).

	
%%
%% ERKLÄRUNG: das hier ist die Zusammenfassung von allem
%% War schon vorgegeben von der Grundstruktur her
%% statistics() startet sowas wie einen Timer;
%% Dann kommt die Berechnung für 3 oder 4, sonst nen error
%% dann nochmal statistics() zum Beenden des Timers
%% Ausgabe von Anzahl Quadrate und Zeit der Rechnung in Sekunden
%% Zum Schluss alle Quadrate ausgeben
%%
%% Wenn man das mit 1 Param auswählt, kommt nur die Berechnung
%% Der Modus wird dann einfach auf egal (ein Atom) gesetzt
%% Dadurch wird combineSquares/3 aufgerufen
%%
%% Wenn man das mit 2 Params auswählt, kann man nen Modus mit angeben
%% Der Modus ist ein Atom; entweder es ist debug für DebugModus
%% oder irgendwas anderes
%% Im Falle des DebugModus wird combineSquares/4 aufgerufen 
%% Der 4. Param ist einfach ein Int, der hochzählt (am Anfang 0)
%% Das führt dazu, das jedes Quadrat einzeln ausgedruckt wird
%%
%% Es fällt auf, das combineRows immer mit 3 Params aufgerufen wird;
%% was zweifelhaft ist, da der Funktionsrumpf von combineRows mit 4 Params
%% vorgegeben war... ist aber nicht weiter tragisch, hab ich halt combineRows/3
%% noch hinzugefügt (siehe oben bei combineRows)
%%
magicsquare(Max) -> magicsquare(Max, egal).
magicsquare(Max, Mode) ->
  statistics(runtime),
  Result = case Mode of
	debug ->  case Max of 
	  3 -> Parts = combineRows(2, 3, 15), combineSquares(Parts, 3, 15, 0);
	  4 -> Parts = combineRows(1, 4, 34), combineSquares(Parts, 4, 34, 0);
	  _ -> erlang:error("Falscher Param für magicsquare!")
	end;
	_ -> case Max of 
	  3 -> Parts = combineRows(2, 3, 15), combineSquares(Parts, 3, 15);
	  4 -> Parts = combineRows(2, 4, 34), combineSquares(Parts, 4, 34);
	  _ -> erlang:error("Falscher Param für magicsquare!")
	end
  end,
  {_, Time1} = statistics(runtime),
  U = Time1 / 1000,
  io:format("Anzahl der Quadrate: ~p~n", [length(Result)]),
  io:format("Magicsquare Time: ~p~n", [U]),
  Result.

  
	
%% ============================================================================
%% HIER BEGINNT DIE VERTEILUNG DES ALGORITHMUS
%% ============================================================================

%% ============================================================================
%% VERTEILUNG AUF EINEM RECHNER
%% ============================================================================

%%
%% Berechnung Magischer Quadrate
%% Funktioniert für N=3 und N=4
%% Aufruf: distribMS(Max, PCount)
%% Max - Anzahl der Reihen/Spalten
%% PCount - Anzahl der Prozesse auf die aufgespalten werden soll
%% wobei wenn X=3 - die Summe ist wird auf 15 gesetzt
%% oder wenn X=4, dann ist die Summe gleich 34
%%
%% spec ist die Funktionsdeklaration (Signatur); distribMS/2 muss 2 Ints als
%% Parameter haben und ne Liste von Integerlisten zurückgeben
%%
%% ERKLÄRUNG: das hier ist praktisch magicsquare/2 von weiter oben
%% War schon vorgegeben von der Grundstruktur her
%% statistics() startet sowas wie einen Timer;
%% Dann kommt die Berechnung für 3 oder 4, sonst einfach leere Liste
%% dann nochmal statistics() zum Beenden des Timers
%% Ausgabe von Anzahl Quadrate und Zeit der Rechnung in Sekunden
%% Zum Schluss alle Quadrate ausgeben
%%
-spec distribMS(posint(), posint())-> list(list(posint())). 
distribMS(Max, PCount)->
  statistics(runtime),
  Result = 
	case Max of
	  3 -> Value = 15, PSquare = combineRows(1, Max, Value),
		spawn_at(PCount, node(), PSquare, Max, Value, init_local),
		loop_gather(PCount, []);	
	  4 -> Value = 34, PSquare = combineRows(2, Max, Value),
	    spawn_at(PCount, node(), PSquare, Max, Value, init_local),
	    loop_gather(PCount, []);
	  _ ->  [[]]	 
	end,
  {_, Time1} = statistics(runtime),
  U = Time1 / 1000,
  io:format("Anzahl der Quadrate: ~p~n", [length(Result)]),
  io:format("Magicsquare Time: ~p~n", [U]),
  Result.

	
%%
%% Spawnt eine festgelegte Anzahl von Prozessen auf einem angegebenen Host
%% Aufruf: spawn_at(CCount, Host, Plist, Max, Value, InitFun)
%% CCount - Anzahl der Prozesse, die abgespalten werden sollen
%% Host - Host auf dem der Prozess erzeugt werden soll / wird in diesem Teil 
%% nicht benötigt, da alles auf dem lokalen Rechner stattfindet
%% InitFun - Funktion, die beim Initialisieren des Prozesses startet
%%
%% WAR VORGEGEBEN ZUM ERWEITERN
%%
%% spec ist die Funktionsdeklaration (Signatur); spawn_at/6 muss 6 
%% Parameter haben und das Atom ok zurückgeben
%%
%% ERKLÄRUNG: Wir rufen spawn_at/7 auf, wobei der 7. Parameter
%% der Counter für spawn ist (am Anfang also 0)
%%
%% die Action passiert in spawnP/7: Abbruchbedingung ist erreicht, wenn
%% CCount = SCount; es wurden alle gespawnt und wir hören auf
%%
%% Ansonsten spawnen wir ständig neue Prozesse mit spawn/4
%% ?MODULE ist nen Makro für den Modulnamen = bel3_2
%%
-spec spawn_at(integer(), atom(), list(list(posint())), posint(), posint(), atom()) -> ok.
spawn_at(CCount, Host, PList, Max, Value, InitFun) ->
  spawn_at(CCount, Host, PList, Max, Value, InitFun, 0).

-spec spawn_at(integer(), atom(), list(list(posint())), posint(), posint(), atom(), posint()) -> ok.
spawn_at(CCount, _, _, _, _, _, SCount) when SCount == CCount -> done;
spawn_at(CCount, Host, PList, Max, Value, InitFun, SCount) ->
  Len = trunc(length(PList) / CCount),
  Rest = case CCount - SCount  of 
    1 -> SCount;
    _ -> 0 end,
  Argus = [SCount, self(), lists:sublist(PList, Len * SCount + 1, Len + Rest), Max, Value, Host],
  Pid = spawn_link(Host, ?MODULE, InitFun, Argus),
  pDebug(Pid, self(), node(), pstart),
  spawn_at(CCount, Host, PList, Max, Value, InitFun, SCount + 1).


%%
%% Methode, die bei Abspaltung des Prozesses aufgerufen wird
%% hat die Parameter [Nr, SPid, PList, Max, Value, Host]
%% Die Methode berechnet für eine Menge an Teilquadraten alle Lösungen und
%% sendet diese an den erzeugenden Prozess.
%% Nr - Nummer des Prozesses (nur für debug-Ausgaben auf der Konsole)
%% SPid - Prozessnummer des ElternProzesses - für das Senden des Ergebnisses
%% PList - Teilliste, für die ein Prozess die magischen Quadrate berechnen soll
%% Max - Anzahl der Spalten/Zeilen
%% Value - Wert der Summe der Zeile 
%% Host - kann hier vernachlaessigt werden 
%%
%% WAR VORGEGEBEN ZUM ERWEITERN
%%
%% spec ist die Funktionsdeklaration (Signatur); init_local/6 muss 6
%% Parameter haben und ein Atom ok zurückgeben
%%
%% ERKLÄRUNG: ruft die distrib_calc_squares/5 auf (vorgegeben)
%% Der 6. Param ist _, weil Host ja nicht benutzt werden soll
%% siehe seinen Text
%% Die ruft für die Teilquadrateliste PList combineSquares/3 auf.
%% Ergebnise werden dann mit dem Atom calc an den Elternprozess gesendet
%% also praktisch an loop_gather
%%
-spec init_local(posint(), pid(), list(list(posint())), posint(), posint(), atom()) -> ok.
init_local(Nr, SPid, PList, Max, Value, _) -> 
  distrib_calc_squares(Nr, SPid, PList, Max, Value).

	
-spec distrib_calc_squares(posint(), pid(), list(list(posint())), posint(), posint()) -> ok.
distrib_calc_squares(_, SPid, PList, Max, Value) -> 
  Erg = combineSquares(PList, Max, Value),
  SPid ! {calc, Erg},
  pDebug(self(), SPid, node(), pende).


%%
%% Methode sammelt alle Ergebnisse ein
%% Wird von der Methode magicsquare/2 aufgerufen
%% Aufruf (CCount, Result)
%% CCount - Anzahl der Prozesse, die gestartet wurden (entspricht Anzahl der
%%		   zu erwartenden Ergebnisse
%% Result - Aktuell bereitstehendes Ergebnis
%%
%% WAR VORGEGEBEN ZUM ERWEITERN
%%
%% spec ist die Funktionsdeklaration (Signatur); loop_gather/2 muss nen Int als
%% und ne Liste von Intlisten als Parameter haben und ne Liste von Intlisten 
%% zurückgeben
%%
%% ERKLÄRUNG: CCount ist der Counter zum Einsammeln
%% loop_gather ruft sich ständig selbst auf mit verringertem Counter
%% in jedem Aufruf wird auf ein RECIEVE gewartet:
%% ein Tupel aus dem Atom calc und der Ergebnisliste
%% Die Liste wird immer mit der letzten konkateniert
%% Wenn der Counter 0 ist, geben wir alles zurück
%%
-spec loop_gather(posint(), list(list(posint()))) -> list(list(posint())).
loop_gather(0, Result) -> pDebug(self(), length(Result), node(), fertig), Result;		
loop_gather(CCount, Result) -> 
	receive
      {calc, Res} -> loop_gather(CCount - 1, Result ++ Res);
	  
	  {'EXIT', _, normal} -> % not a crash
        io:format("Normal!~n", []), loop_gather(CCount, Result);
		
      {'EXIT', _, shutdown} -> % manual shutdown, not a crash
        io:format("Shutdown!~n", []), loop_gather(CCount, Result);
		
      {'EXIT', Pid, _} ->
        io:format("~p ist abgestürzt! Wir sind in ~p in Node ~p~n ", [Pid, self(), node()]),
		host_monitor ! {getnode, self()},
			receive
			  {newhost, Neu} -> io:format("Neu: ~p~n", [Neu]), Result
			  after 3000 -> io:format("dann halt nicht ~n", []), Result
			end
    end.


%% ============================================================================
%% VERTEILUNG AUF MEHRERE RECHNER 
%% ============================================================================

%% ============================================================================
%% Vorgegebene Hilfsfunktionen
%% ============================================================================

%%
%% Codieren der Hostnamen mit Anzahl von Prozessen, die sie ausführen sollen
%%
%hosts() -> [{'tiger@hadoop03', 48}, {'scorpion@hadoop06', 48}].

hosts() -> [{'antonyn@localhost', 2}, {'helene@localhost', 2}].


%%
%% Berechnung der Anzahl der Prozesse insgesamt
%% Soll fuer die Aufteilung der Quadrate verwendet werden
%%
c_count() -> lists:sum([Count|| {_,Count} <- hosts()]).


%% ============================================================================
%% Beginn Berechnung Magischer Quadrate
%% ============================================================================

%%
%% Funktioniert fuer N=3 und N=4
%% Aufruf: distribMS(Max, PCount)
%% Max - Anzahl der Reihen/Spalten
%% PCount - Anzahl der Prozesse auf die aufgespalten werden soll
%% wobei wenn X=3 - die Summe ist wird auf 15 gesetzt
%% oder wenn X=4, dann ist die Summe gleich 34
%%
%% WAR VORGEGEBEN ZUM ERWEITERN
%%
%% spec ist die Funktionsdeklaration (Signatur); megaDistribMS/1 muss 1 Int als
%% Parameter haben und eine Liste von Intlisten zurückgeben
%%
%% ERKLÄRUNG: 
%% damit es zuhause funzt: in Beleg3 Ordner cmd aufmachen und ausführen:
%% werl -sname starter@localhost
%% werl -sname antonyn@localhost
%% werl -sname helene@localhost
%%
%% im Werl Fenster von starter macht man dann: bel3_2:megaDistribMS(4).
%% weil er versucht ja, einen prozess auf die Nodes zu spawnen
%% d.h. damit das geht, müssen die Nodes ja vorhanden sein
%% Also starte ich die Nodes in eigenen Werl Fenstern und lass sie auf
%% Wenn er jetzt einen antonyn@localhost sucht, findet er ihn
%%
%% Ansonsten ist das hier praktisch magicsquare/2 von weiter oben
%% War schon vorgegeben von der Grundstruktur her
%% Erst starten wir den Monitoring-Prozess
%% statistics() startet sowas wie einen Timer;
%% Dann kommt die Berechnung für 3 oder 4, sonst einfach leere Liste
%% dann nochmal statistics() zum Beenden des Timers
%% Ausgabe von Anzahl Quadrate und Zeit der Rechnung in Sekunden
%% Dann senden wir stop an den Monitoring-Prozess
%% Zum Schluss alle Quadrate ausgeben
%%
%% Der Workflow ist praktisch:
%% Hauptprozess spawnt X neue Prozesse, wobei X Anzahl Hosts ist
%% (das passiert per while/5)
%% Jeder dieser X Prozesse startet mit der Funktion init_global
%% Diese Funktion spawnt Y neue Prozesse, wobei Y Anzahl Prozesse ist
%% (das passiert per spawn_at/6 in init_global/6)
%% Jeder dieser Y Prozesse startet mit der Funktion init_local
%% Diese Funktion ruft combineSquares/3 auf
%% Die Ergebnisse werden dann an den Elternprozess gesendet
%% d.h. an die X Prozesse 
%% Die Ergebnisse der X Prozesse werden dann an den Hauptprozess gesendet
%% (das passiert durch loop_gather/2 in megaDistribMS/1)
%%
megaDistribMS(Max)->
	
  % Ausschalten des Error-Loggings auf der Konsole
  error_logger:tty(false),
  register(host_monitor, spawn(fun() -> init_host_monitor(hosts()) end)),
  statistics(runtime),
  Result = 
    case Max of
	
	  3 -> Value = 15, PSquare = combineRows(2, Max, Value),
		while(length(hosts()), hosts(), PSquare, Max, Value),
		loop_gather(length(hosts()), []);
		
	  4 -> Value = 34, PSquare = combineRows(2, Max, Value),
		while(length(hosts()), hosts(), PSquare, Max, Value),
		loop_gather(length(hosts()), []);
		
	  _ ->  [[]]	 
	end,
  {_, Time1} = statistics(runtime),
  U = Time1 / 1000,
  io:format("Anzahl der Quadrate: ~p~n", [length(Result)]),
  io:format("Magicsquare Time: ~p~n", [U]),
  host_monitor ! stop,
  Result.

	
%%
%% Schleife fuer das spawnen der Prozesse auf mehreren Rechnern
%% Benutzt die Methode spawn_at(...)
%% Aufruf: while (CCount, Hosts, PList, Max, Value)
%% CCount - Anzahl der Prozesse die gespawnt werden sollen
%% Hosts - Hostliste der Form { VM-Name, Anzahl der Prozesse}
%% PList - Liste der Teilquadrate
%% Max - Anzahl der Elemente, die berechnet werden sollen
%% Value - Wert der Summe der Zeile
%%
%% WAR VORGEGEBEN ZUM ERWEITERN
%%
%% spec ist die Funktionsdeklaration (Signatur); while/5 muss 5
%% Parameter haben und ein Atom ok zurückgeben
%%
%% ERKLÄRUNG: ruft die while/7 auf, die alles macht:
%% Der 6. Param ist am Start 0, weil das der Counter für Hosts ist
%% Der 7. Param ist die Länge, für die Zerteilung der Listen
%% Man muss ja die Eingabelisten aufteilen auf alle Prozesse/Nodes,
%% deshalb teilen wir erstmal Listenlänge durch Anzahl Hosts
%% trunc(), um eine Ganzzahl zu garantieren
%%
%% 1. Fall ist Abbruchbedingung: Wenn nur noch ein Host da ist -> Ende
%% Ansonsten: per Pattern Matching holen wir Nodename und deren Prozessanzahl;
%% spawn_at wird immer aufgerufen mit init_global als Startfunktion
%% Nach jedem spawn_at wird HostCount erhöht, bis zum Abbruch
%%
%% Wenn nur noch 1 Host in Hosts, hol per nthtail() den Rest aus Plist
%% Wenn mehrere Hosts, nimm den 1. Host, und hol per sublist() einen Teil aus
%% PList rausgegangen
%% z.B. für 2 Hosts bei 3x3 haben wir 432 Listen in PList, also 
%% machen wir einmal von 1 bis 432/2
%% und dann von 432/2 + 1 bis Ende
%%
-spec while(posint(), list({atom(), posint()}), list(list(posint())), posint(),
            posint()) -> ok.
while (CCount, Hosts, PList, Max, Value) -> process_flag(trap_exit, true), 
  while(CCount, Hosts, PList, Max, Value, 0, trunc(length(PList) / length(hosts()))).
  
 
-spec while(posint(), list({atom(), posint()}), list(list(posint())), posint(),
            posint(), posint(), posint()) -> ok. 
while(_, [{ VMName, _}], PList, Max, Value, HostCount, Len) -> 
  spawn_at(1, VMName, lists:nthtail(Len * HostCount, PList), Max, Value, init_global); 
  
while(CCount, [{ VMName, _} | HostsR], PList, Max, Value, HostCount, Len) -> 
  Hcount = length(hosts()), 
  Rest = case length(HostsR) of 
    0 -> Hcount - 1;
    _ -> 0 end,
  spawn_at(1, VMName, lists:sublist(PList, Len * HostCount + 1, Len + Rest), Max, Value, init_global), 
  while(CCount, HostsR, PList, Max, Value, HostCount + 1, Len).


%%
%% Supervisor-Prozess, der die Ausführung der Berechnungen überwacht
%% Spawnt die Berechnungsprozesse auf den Nodes des Erlang-Clusters und 
%% behandelt die Fehlerfälle
%% Nr - Nummer des Prozesses (nur zur besseren Identifikation)
%% SPid - Prozessnummer des erzeugenden Prozesses
%% PList - Teilliste, für die ein Prozess die magischen Quadrate berechnen soll
%% Max - Anzahl der Spalten/Zeilen
%% Value - Wert der Summe der Zeile 
%% Try - Anzahl der noch ausstehenden Versuche
%%
%% WAR VORGEGEBEN ZUM ERWEITERN
%%
%% spec ist die Funktionsdeklaration (Signatur); init_global/6 muss 6
%% Parameter haben und ein Atom ok zurückgeben
%%
%% TODO: Try hat noch keine Wirkung, implementier das mal!
%% So nach dem Prinzip: wenn spawn_at/6 fehlschlägt, oder wenn
%% irgendwie ein "Node down!" reinkommt, dann rufe rekursiv auf mit Try - 1
%% Wenn Try dann 0 ist, mach irgendwas?!
%%
%% ERKLÄRUNG: ruft init_global/7 auf, die alles macht:
%% Der 7. Param ist die Anzahl der Trys, hier 3 von ihm vorgegeben
%%
%% Erst holen wir per Pattern Matching die Anzahl Prozesse des Hosts
%% Genauso oft rufen wir dann spawn_at mit der init_local auf
%% Dann warten wir auf die Ergebnisse mit loop_gather/2
%% Die Ergebnisliste Result wird dann an den Elternprozess gesendet
%% (d.h. an das loop_gather/2 in megaDistribMS/1)
%%
-spec init_global(posint(), pid(), list(list(posint())), posint(), posint(), 
				  atom()) -> ok.
init_global(Nr, SPid, PList, Max, Value, Host) -> 
  init_global(Nr, SPid, PList, Max, Value, Host, 3).

-spec init_global(posint(), pid(), list(list(posint())), posint(), posint(), 
                  atom(), posint()) -> ok.	
  init_global(_, SPid, PList, Max, Value, Host, Try) ->
	process_flag(trap_exit, true),  
    {_, PCount} = lists:keyfind(Host, 1, hosts()),
    spawn_at(PCount, Host, PList, Max, Value, init_local),
	
    Result = loop_gather(PCount, []), 
	
    SPid ! {calc, Result},
	pDebug(self(), SPid, node(), pende).


%% ============================================================================
%% Vorgegeben für Monitoring von Prozessen
%% ============================================================================

%%
%% Monitoring-Prozess fuer die Überwachung der zur Verfügung stehenden 
%% Cluster-Nodes. Er wird von der Hauptmethode megaDistribMS gestartet,
%% Der Prozess kann über das Atom host_monitor angesprochen werden.
%% Er beinhaltet die folgenden Operationen:
%%  getnode - Ermittlung eines verfügbaren Nodes
%%  addnode - Hinzunahme eines Nodes
%%  gethosts - Ermittlung aller verfügbaren Hosts
%%  deletenode - Löschen eines Nodes
%%
init_host_monitor(MonitorList) -> 
  ML = lists:map(fun({Host, _}) -> Host end, MonitorList),
  lists:foreach(fun(Host) -> erlang:monitor_node(Host, true) end, ML),
  monitorHosts(ML).
	
	
monitorHosts([]) -> erlang:error(no_hosts_available);
monitorHosts(HostList) -> 
  receive
	{nodedown, NodeName} -> io:format("Host ~p is down!~n", [NodeName]),
	  monitorHosts(lists:delete(NodeName, HostList));

	{getnode, From} -> io:format("Host ~p is requested!~n", [hd(HostList)]),
	  From ! {newhost, hd(HostList)}, 
	  monitorHosts(tl(HostList) ++ [hd(HostList)]);

	{addnode, NodeName} -> io:format("Host ~p is added!~n", [NodeName]),
	  monitor_node(NodeName, true),
	  monitorHosts([NodeName|HostList]);
	  
	{gethosts, From} -> From ! {hostlist, HostList}, monitorHosts(HostList);

	{deletenode, NodeName}-> 
	  io:format("Host ~p will be deleted!~n", [NodeName]),
	  monitorHosts(lists:delete(NodeName, HostList));

	stop -> ok 
  end.
