%%% coding: utf-8
%%% @author CHRIS
%%% @doc Belegaufgabe 3
-module(bel3_2).
-compile(export_all). % alle Methode "public" machen


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


%% Berechnet alle moeglichen Zeilen eines Magischen Quadrats
%% Aufruf: row(Max, Value, Elements) - z.B. row(3,15,lists:seq(1,15))
%% Max - Seitengroesse des Quadrats
%% Value - Wert der Summe der Zeile
%% Elements - Elemente aus denen ausgewaehlt werden soll
%%
%% spec ist die Funktionsdeklaration (Signatur); combinations/1 muss ne
%% Tupelliste als Parameter haben und ne Liste von Tupellisten zurückgeben
%%
%% ERKLÄRUNG: Mit 4 Params aufrufen, um Max "speichern" zu können
%% im List generator wird Max ja verringert, also müssen wir die ursprüngliche 
%% Größe irgendwie speichern
%% Dann List Generator
%%
-spec row(posint(), posint(), list(posint())) -> list(list(posint())).
row(Max, Value, Elements) -> row(Max, Max, Value, Elements). 
row(0, _, _, _) -> [[]];
row(Max, MaxGes , Value, Elements) ->  
	[Zeil ++ [E] || 
		Zeil <- row(Max - 1, MaxGes, Value, Elements),
		E <- Elements -- Zeil,
		(Max /= MaxGes) or ((lists:sum(Zeil ++ [E])) == Value)].


%% Funktion, die ermittelt, ob sich in zwei Listen doppelte Elemente befinden
%% ERKLÄRUNG: Die Listen werden zusammengepackt und dann sortiert, d.h.
%% wenn es gleiche gibt, sind die jetzt direkt nebeneinander
%% Eine Hilfsfunktion geht dann einfach durch und prüft die ersten beiden
%% Abbruchbedingung ist leere Liste.
%%
%% spec ist die Funktionsdeklaration (Signatur); duplicheck/1 muss ne
%% Liste von Ints als Parameter haben und nen BOOLEAN zurückgeben
%% das hier ist die Hilfsfunktion für duplicate/2
%%
-spec duplicheck(list(posint())) -> boolean().
duplicheck([]) -> false;
duplicheck([E, E| _]) -> true;
duplicheck([_| R]) -> duplicheck(R).


%%
%% spec ist die Funktionsdeklaration (Signatur); duplicate/2 muss zwei
%% Listen von Ints als Parameter haben und nen BOOLEAN zurückgeben
%%
-spec duplicate(list(posint()), list(posint())) -> boolean().
duplicate(L1, L2) -> duplicheck(lists:sort(L1 ++ L2)).


%% combineRows setzt eine beliebige Anzahl von Reihen, die vorab berechnet
%% werden, zusammen. Dabei wird ueberprueft, ob sich doppelte Elemente 
%% innerhalb der Reihen befinden. Aufruf: combineRows (Col, Max, Value)
%% Col - Anzahl der Reihen, die berechnet werden sollen
%% Max - Anzahl der Elemente pro Zeile
%% Value - Wert der Summe der Zeile
%% Elems - Elemente, aus denen gewaehlt werden soll

-spec combineRows(posint(), posint(), posint(), list(posint())) -> list(list(posint())).
combineRows(Col, Max, Value, Elems) -> combineRows(Col, row(Max, Value, Elems)). 

combineRows(0, _) -> [[]];
combineRows(Col, Zeilen) -> 
  [CR ++ Z || 
    CR <- combineRows(Col - 1, Zeilen),  
    Z <- Zeilen, 
    duplicate(CR, Z) == false ].


combineRows(Col, Max, Value) -> combineRows(Col, Max, Value, lists:seq(1, Max * Max)). 
	
	
%% calcSquares berechnet aus einem Teilquadrat alle moeglichen gueltigen Quadrate, die sich bilden lassen
%% Aufruf: calcSquares(Part, Max, Value)
%% Part - Teilquadrat fuer das die Magischen Quadrate berechnet werden sollen
%% Max - Anzahl der Elemente pro Zeile/Spalte
%% Value - Wert der Summe einer Zeile
-spec calcSquares(list(posint()), posint(), posint()) -> list(list(posint())).
calcSquares(Part, Max, Value) -> 
	RestZeil =  (Max - length(Part) div Max),
	[Part ++ X || X <- combineRows(RestZeil, Max, Value, lists:seq(1, Max * Max) -- Part), istMagisch(Part ++ X, Max, Value)].


%%% istMagisch checkt folgendes: Diagonale(\) = Value UND Diagonale(/) = Value	UND Summe in jeder Spalte = Value

%% erstellt aus einer Liste von Zeilen eine Liste von Spalten; aus [ [1,2,3],[4,5,6],[7,8,9] ] wird [ [1,4,7],[2,5,8],[3,6,9] ]
% head/1 holt sich die Köpfe (1,4,7) und pack das vorne ran...dann nochmal aufrufen mit den tails
transpose([[]|_]) -> [];
transpose(Quadrat) ->
  [lists:map(fun hd/1, Quadrat) | transpose(lists:map(fun tl/1, Quadrat))].
  

%% überprüft,ob das Quadrat Quad mit Seitenlänge Max ein Magisches Quadrat ist (mit magischer Zahl Val)
istMagisch(Quad, Max, Val) ->
% umwandeln des Zeilen-Quadrates in ein Spalten-Quadrat (Siehe transpose)
Spalten = transpose(Quad),
% alle Spalten rausnehmen, deren Summe der magischen Zahl Val entspricht
KorrekteSpalten = lists:takewhile(fun(X) -> lists:sum(X) == Val end, Spalten),
		
% wenn es genauso viele Spalten waren wie Seitenlänge des Quadrates, check auch die Diagonalen
% wir checken also erst ob alle 3 Spalten 15 ergeben (z.b.)... wenn sie das tun, ist die case
% abfrage TRUE, und wir checken die Diagonalen, ob die auch jeweils 15 sind
% nur wenn beide TRUE, ist gesamt TRUE, wegen dem AND
% wenn die spalten schon falsch waren, brauchen wir die diagonalen gar nich erst checken
% kann ja eh kein magisches Quadrat mehr werden
case length(KorrekteSpalten) == Max of
  true -> checkDiaLR(Quad, Max, Val);
  false -> false
end.

%% überprüft, ob die Summe der LR-Diagonale des Quadrates Q mit Seitenlänge M gleich V ist
checkDiaLR(Q, M, V) -> QFlach = lists:flatten(Q),
case V == lists:foldl(fun(Z, Dia) -> Dia + lists:nth((M * (Z - 1) + Z), QFlach) end, 0, lists:seq(1, M)) of
  true -> checkDiaRL(Q, M, V);
  false -> false
end.

%% überprüft, ob die Summe der RL-Diagonale des Quadrates Q mit Seitenlänge M gleich V ist  
checkDiaRL(Q, M, V) -> QFlach = lists:flatten(Q),
  V == lists:foldl(fun(Z, Dia) -> Dia + lists:nth(((M - 1) * Z + 1), QFlach) end, 0, lists:seq(1, M)).

%% combineSquares ermittelt aus allen Teilquadraten die gueltige Loesung
%% Aufruf: combineSquares(Parts, Max, Value)
%% Parts - Alle Teilquadrate
%% Max - Anzahl der Zeilen
%% Value - Wert der Summe einer Zeile
-spec combineSquares(list(list(posint())), posint(), posint(), integer())->list(list((posint()))).
combineSquares([], _, _, _) -> [];
combineSquares([X| XS], Max, Value, Num) ->
	Res = calcSquares(X, Max, Value),
	case Res of 
		[] -> combineSquares(XS, Max, Value, Num);
		_ ->	io:format("Erg Nummer~p:~p~n",[Num, Res]),Res ++ combineSquares(XS, Max, Value, Num + length(Res))
	end.

-spec combineSquares(list(list(posint())), posint(), posint())->list(list((posint()))).
combineSquares(Parts, Max, Value) ->
	lists:flatmap(fun(X) -> calcSquares(X, Max, Value) end, Parts).


magicsquare(Max)-> magicsquare(Max, egal).
magicsquare(Max, Mode)->
	statistics(runtime),
	Result = case Mode of
			debug ->  case Max of 
					3 -> Parts = combineRows(2, 3, 15), combineSquares(Parts, 3, 15, 0);
					4 -> Parts = combineRows(1, 4, 34), combineSquares(Parts, 4, 34, 0);
					_-> error
				end;
			_ -> case Max of 
					3 -> Parts = combineRows(2, 3, 15), combineSquares(Parts, 3, 15);
					4 -> Parts = combineRows(2, 4, 34), combineSquares(Parts, 4, 34);
					_-> error
				end
	end,
	{_, Time1} = statistics(runtime),
	U = Time1 / 1000,
	io:format("Anzahl der Quadrate:~p~n", [length(Result)]),
	io:format("Magicsquare Time:~p~n", [U]),
	Result.

%% ============================================================================
%% HIER BEGINNT DIE VERTEILUNG DES ALGORITHMUS
%% ============================================================================

%% ============================================================================
%% VERTEILUNG AUF EINEM RECHNER
%% ============================================================================

%% Berechnung Magischer Quadrate
%% Funktioniert fuer N=3 und N=4
%% Aufruf: distribMS(Max, PCount)
%% Max - Anzahl der Reihen/Spalten
%% PCount - Anzahl der Prozesse auf die aufgespalten werden soll
%% wobei wenn X=3 - die Summe ist wird auf 15 gesetzt
%% oder wenn X=4, dann ist die Summe gleich 34
-spec distribMS(posint(), posint())-> list(list(posint())). 
distribMS(Max, PCount)->
	statistics(runtime),
	Result= 
		case Max of
			3 -> Value=15, PSquare=combineRows(1,Max,Value),
				spawn_at(PCount, node(), PSquare, 3, Value, init_local),
				loop_gather(PCount,[]);	
			4 -> Value=34, PSquare=combineRows(2,Max,Value),
				spawn_at(PCount, node(), PSquare, 4, Value, init_local),
				loop_gather(PCount,[]);
			_ ->  [[]]	 
		end,
	{_, Time1} = statistics(runtime),
	U= Time1/ 1000,
	io:format("Anzahl der Quadrate:~p~n",[length(Result)]),
	io:format("Magicsquare Time:~p~n",[U]),
	Result.

%% Spawnt eine festgelegte Anzahl von Prozessen auf einem angegebenen Host
%% Aufruf: spawn_at(CCount, Host, Count, Plist, Max, Value)
%% CCount - Anzahl der Prozesse, die abgespalten werden sollen
%% Host - Host auf dem der Prozess erzeugt werden soll / wird in diesem Teil nicht benoetigt,
%% 		 da alles auf dem lokalen Rechner stattfindet
%% InitFun - Funktion, die beim Initialisieren des Prozesses aufgerufen werden soll
-spec spawn_at(integer(), atom(), list(list(posint())), posint(), posint(), atom()) -> ok.
spawn_at(CCount, Host, PList, Max, Value, InitFun)-> toBeDefined.

%% Methode, die bei Abspaltung des Prozesses aufgerufen wird
%% hat die/den Parameter [Nr, SPid, PList, Max, Value, Host]
%% Die Methode berechnet fuer eine Menge an Teilquadraten alle Loesungen und
%% sendet diese an den erzeugenden Prozess.
%% Nr - Nummer des Prozesses (nur fuer debug-Ausgaben auf der Konsole)
%% SPid - Prozessnummer des erzeugenden Prozesses - fuer das Senden des Ergebnisses
%% PList - Teilliste, fuer die ein Prozess die magischen Quadrate berechnen soll
%% Max - Anzahl der Spalten/Zeilen
%% Value - Wert der Summe der Zeile 
%% Host - kann hier vernachlaessigt werden 
init_local(Nr, SPid, PList, Max, Value,_)-> 
	distrib_calc_squares(Nr, SPid, PList, Max, Value).

-spec distrib_calc_squares(posint(), pid(), list(list(posint())), posint(), posint()) -> ok.
distrib_calc_squares(Nr, SPid, PList, Max, Value)-> toBeDefined.

%% Methode sammelt alle Ergebnisse ein
%% Wird von der Methode magicsquare aufgerufen
%% Aufruf (CCount, Result)
%% CCount - Anzahl der Prozesse, die gestartet wurden (entspricht der Anzahl der
%%		   zu erwartenden Ergebnisse
%% Result - Aktuell bereitstehendes Ergebnis

-spec loop_gather(posint(), list(list(posint())))-> list(list(posint())).
loop_gather(CCount,Result)-> toBeDefined.

%% ============================================================================
%% VERTEILUNG AUF MEHERE RECHNER 
%% ============================================================================

%% Codieren der Hostnamen mit der Anzahl von Prozessen, die sie ausfuehren sollen
hosts()->[{'tiger@hadoop03',48},{'scorpion@hadoop06',48}].

%% Berechnung der Anzahl der Prozesse insgesamt
%% Soll fuer die Aufteilung der Quadrate verwendet werden
c_count()-> lists:sum([Count||{_,Count}<-hosts()]).


%% Berechnung Magischer Quadrate
%% Funktioniert fuer N=3 und N=4
%% Aufruf: distribMS(Max, PCount)
%% Max - Anzahl der Reihen/Spalten
%% PCount - Anzahl der Prozesse auf die aufgespalten werden soll
%% wobei wenn X=3 - die Summe ist wird auf 15 gesetzt
%% oder wenn X=4, dann ist die Summe gleich 34

megaDistribMS(Max)->
	
	% Ausschalten des Error-Loggings auf der Konsole
	error_logger:tty(false),
	register(host_monitor,spawn(fun()->init_host_monitor(hosts()) end)),
	statistics(runtime),
	Result= 
		case Max of
			3 -> Value=15, PSquare=combineRows(2,Max,Value),
				while(c_count(), hosts(), PSquare, 3, 15), 
				%spawn_at(4, node(), PSquare, 3, Value),
				loop_gather(c_count(),[]);	
			4 -> Value=34, PSquare=combineRows(2,Max,Value),
				while(c_count(), hosts(), PSquare, 4, 34),
				%spawn_at(4, node(), PSquare, 4, Value),
				loop_gather(c_count(),[]);
			_ ->  [[]]	 
		end,
	{_, Time1} = statistics(runtime),
	U= Time1/ 1000,
	io:format("Anzahl der Quadrate:~p~n",[length(Result)]),
	io:format("Magicsquare Time:~p~n",[U]),
	host_monitor!stop,
	Result.

%% Schleife fuer das spawnen der Prozesse auf mehreren Rechnern
%% Benutzt die Methode spawn_at(...)
%% Aufruf: while (CCount, Hosts, PList, Max, Value)
%% CCount - Anzahl der Prozesse die gespawnt werden sollen
%% Hosts - Hostliste der Form { VM-Name, Anzahl der Prozesse}
%% PList - Liste der Teilquadrate
%% Max - Anzahl der Elemente, die berechnet werden sollen
%% Value - Wert der Summe der Zeile
-spec while(posint(), list({atom(),posint()}), list(list(posint())), posint(),posint())->ok.
while (CCount, HostCountL, PList, Max, Value) -> toBeDefined.
	
%% Supervisor-Prozess, der die Ausfuehrung der Berechnungen ueberwacht
%% Spawnt die Berechnungsprozesse auf den Nodes des Erlang-Clusters und behandelt die Fehlerfaelle
%% Nr - Nummer des Prozesses (nur zur besseren Identifikation)
%% SPid - Prozessnummer des erzeugenden Prozesses
%% PList - Teilliste, fuer die ein Prozess die magischen Quadrate berechnen soll
%% Max - Anzahl der Spalten/Zeilen
%% Value - Wert der Summe der Zeile 
%% Try - Anzahl der noch ausstehenden Versuche

init_global(Nr, SPid, PList, Max, Value, Host)->
	init_global(Nr, SPid, PList, Max, Value, Host,3).

-spec init_global(posint(), pid(), list(list(posint())), posint(), posint(),
	atom(), posint()) -> ok.	
init_global(Nr, SPid, PList, Max, Value, Host, Try)-> toBeDefined.

%% Monitoring-Prozess fuer die Ueberwachung der zur Verfuegung stehenden Cluster-Nodes
%% Er wird von der Hauptmethode megaDistribMS gestartet,
%% Der Prozess kann ueber das Atom host_monitor angesprochen werden.
%% Er beinhaltet die folgenden Operationen:
%%  getnode - Ermittlung eines verfuegbaren Nodes
%%  addnode - Hinzunahme eines Nodes
%%  gethosts - Ermittlung aller verfuegbaren Hosts
%%  deletenode - Loeschen eines Nodes

init_host_monitor(MonitorList) -> ML= lists:map(fun({Host,_})->Host end, MonitorList),
	lists:foreach(fun(Host)->erlang:monitor_node(Host, true) end, ML),
	monitorHosts(ML).
	
monitorHosts([])-> erlang:error(no_hosts_available);
monitorHosts(HostList)-> 
	receive
		{nodedown, NodeName}-> io:format("Host ~p is down!~n",[NodeName]),
			monitorHosts(lists:delete(NodeName, HostList));
		{getnode, From}-> io:format("Host ~p is requested!~n",[hd(HostList)]),
			From!{newhost, hd(HostList)}, monitorHosts(tl(HostList)++[hd(HostList)]);
		{addnode, NodeName}-> io:format("Host ~p is added!~n",[NodeName]),
			monitor_node(NodeName, true),
			monitorHosts([NodeName|HostList]);
		{gethosts, From} -> From!{hostlist, HostList}, monitorHosts(HostList);
		{deletenode, NodeName}-> io:format("Host ~p will be deleted!~n",[NodeName]),
			monitorHosts(lists:delete(NodeName, HostList));
		stop -> ok 
	end.
