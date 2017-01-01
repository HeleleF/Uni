%%% coding: utf-8
%%% @author CHRIS
%%% @doc Belegaufgabe 2
-module(bel2).

-compile(export_all). % alle Methode "public" machen


%% ============================================================================
%% TYP-DEFINITIONEN
%% ============================================================================
%% 
%% Type occurenceList repraesentiert eine Menge von Buchstaben, die in einem 
%% Wort vorkommen. Die Buchstabenvorkommen werden als Tupel repraesentiert, mit
%% erster Wert: Buchstabe und zweiter Wert: Anzahl des Vorkommen.
%% occurenceLists sind immer nach der alphabetischen Reihenfolge der Buchstaben 
%% aufsteigend sortiert.
%%
%% das ist praktisch die Tupelliste vom HuffmanTree [{a,1}, {b,1}, {c,1}, ...]
%%
-type occurrenceList() :: list({char(), non_neg_integer()}).


%%
%% In Erlang ist ein String eine Liste von Buchstaben (chars)
%% ein String ist dem Kontext hier ein "Wort"
%% dient nur dazu, das man nicht soviele Klammern etc schreiben muss
%%
-type wort() :: list(char()).


%% ============================================================================
%% FUNKTIONEN 
%% ============================================================================
%%
%% extractLetters/1 bildet eine Kette von Zahlen auf die moeglichen 
%% Buchstabenkombinationen ab. extractLetters/1 bekommt als Parameter die 
%% Zahlenkette und liefert als Ergebnis die Buchstabenkombinationen, die sich 
%% gemaess der Zuordnung bilden lassen
%%
%% spec ist die Funktionsdeklaration (Signatur); extractLetters/1 muss ne Liste
%% von Ints als Parameter haben und eine Liste von Charlisten zurückgeben
%%
%% ERKLÄRUNG: 1. Fall ist Abbruchbedingung: Keine Zahlenkette rein, 
%% also eine leere Liste von Listen raus
%% 2. Fall: Nimm erste Ziffer (als Char aber!) und hol davon die Buchstaben per 
%% gegebener Funktion assignChar
%% Der Rest des Ziffernstrings wird rekursiv aufgerufen und per List Generator 
%% zusammengepackt; das ganze wird dann noch sortiert für den Test
%% (ne extra fun() braucht lists:sort nicht, weil standardmäßig von klein 
%% nach groß sortiert wird)
%%
-spec extractLetters(list(non_neg_integer())) -> list(wort()).
extractLetters([]) -> [ [] ];
extractLetters([A | R]) ->
  lists:sort([Q ++ [C] || C <- assignChar(A), Q <- extractLetters(R)]).
  
  
%%
%% letterOccurences errechnet die Haeufigkeit der Buchstabenvorkommen.
%% letterOccurences bekommt als Parameter eine Zeichenkette und berechnet,
%% welcher Buchstabe wie haeufig vorkommt. Ergebnis ist eine Liste von Tupeln,
%% bestehend aus dem jeweiligen Buchstaben und der Anzahl der Vorkommen.
%% Die Tupelliste muss alphabetisch nach dem Buchstaben geordnet sein.
%% Vervollstaendigen Sie die Funktion splitter, die in der Faltungsfunktion 
%% angewendet wird.  
%%
%% spec ist die Funktionsdeklaration (Signatur); splitter/2 nen Char und ne 
%% Tupelliste als Params bekommen und eine Liste von Zweiertupeln zurückgeben
%% (eine Occurencelist also)
%%
%% ERKLÄRUNG: ist letztendlich das gleiche wie addLetter/2 beim HuffmanTree
%% 1. Fall: Tupelliste ist leer -> also mach den Buchstaben C in nen Tupel rein
%% 2. Fall: 1. Tupel der Tupelliste matched mit Buchstabe C 
%% -> erhöhe dessen Auftreten um 1}
%% 3. Fall: 1. Tupel matched NICHT, also mit dem Rest der Tupelliste aufrufen;
%% neue Tupel werden immer hinten angehängt, um die Sortierung zu erhalten
%% es wird klar, dass 1. Fall die Abbruchbedingung ist: wenn Liste leer -> 
%% mach nen neuen Tupel
%%
-spec splitter(char(), occurrenceList()) -> occurrenceList().
splitter(C, []) -> [{C, 1}];
splitter(C, [{C, I} | RS]) -> [{C, I + 1} | RS];
splitter(C, [E | RS]) -> [E | splitter(C, RS)].


%%
%% spec ist die Funktionsdeklaration (Signatur); letterOccurences/1() muss ne 
%% Charliste als Parameter haben und eine Liste von Zweiertupeln zurückgeben 
%% (eine Occurencelist also)
%%
%% ERKLÄRUNG: letztendlich das gleiche wie createFrequencies/1 beim HuffTree
%% an sich schon alles da: SList ist die Charliste sortiert. 
%% ("afedcfba" macht also "aabcdeff")
%% OccList enthält dann die ausgezählte Tupelliste: 
%% Fold Left reduziert von Links nach Rechts; also für SList = "abc" haben wir
%% dann: splitter("c", splitter("b", splitter("a", "")))
%% Bedeutet also: splitter() wird aufgerufen mit den Params 
%% (Anfangsbuchstabe aus SList) und ""; "" ist dabei die leere Liste []
%% Dabei kommt also raus: [{a,1}]. 
%% Dann wird splitter() aufgerufen mit "b" und [{a,1}]
%% Dabei kommt dann raus: [{a,1}, {b,1}]. 
%% Dann wird splitter() aufgerufen mit "c" und [{a,1}, {b,1}]
%% Dann kommt final raus: [{a,1}, {b,1}, {c,1}]. 
%% Und das ist dann in OccList drinne
%%
%% Eigentlich war hier noch nen lists:reverse drin (wegen fold left prob.), 
%% aber das brauchen wir nicht, weil: bei splitter/2 immer hinten angehangen
%% wird (3. Fall) und die Liste ja sortiert reinkommt, d.h.:
%% "aaabbbbcccc" bedeutet wir erhöhen erst die "a", 
%% dann hängen wir "b" hinten an, erhöhen es, und hängen "c" an.
%% deshalb lassen wa dat weg, oki?
%% Und damit wir keine "Variable Unbound"-Warning bekommen, 
%% nehmen wir OccList auch raus
%%
-spec letterOccurences(wort()) -> occurrenceList().  
% letterOccurences(Word) -> 
%   SList = lists:sort(Word), OccList = lists:foldl(fun splitter/2, "", SList),
%   lists:reverse(OccList).
letterOccurences(Word)-> 
  SList = lists:sort(Word), lists:foldl(fun splitter/2, "", SList).


%%
%% groupBy indexiert eine Liste von beliebigen Elementen mit Hilfe einer zu 
%% uebergebenden Indexierungsfunktion. Die Funktion groupBy bekommt als 
%% Parameter die Liste sowie die Indexierungsfunktion. Bei der Gruppierung
%% werden alle Elemente, die den selben Wert bei der Anwendung der 
%% Gruppierungsfunktion produzieren, in einer Liste zusammengefasst und dem 
%% Funktionswert als Schluessel zugeordnet. So soll bspw. der Aufruf von 
%% groupBy(fun(X)->length(X) end, ["Hallo", "das", "ist", "ein", "Test"]) 
%% die Liste nach der Laenge der Woerter zusammenfassen. Das Ergebnis ist also:
%% [{3->["das","ist","ein"],{4->"Test"},{5->"Hallo"}].
%% Die Map soll in einer Datenstruktur namens dict (siehe Erlang-Dokumentation)
%% gespeichert werden.
%%
%% spec ist die Funktionsdeklaration (Signatur); groupBy/2() muss eine 
%% Funktion/1 und ne Liste als Parameter haben und ein Erlang-dict zurückgeben,
%% für das gilt: Values sind die Listenelemente (A); Keys sind Ergebnisse von 
%% der Funktion fun() angewandt auf die Listenelemente (B)
%%
%% ERKLÄRUNG: Fold Left reduziert Links nach Rechts, also: 
%% groupBy(fun(X)->length(X) end, ["das", "ist", "Test"]):
%% Wir haben also: append("Test", append("ist", append("das", dict:new)))
%% 1. Ein neues dict machen mit dem Key (Länge von "das") und dem Value "das".
%% 2. Jetzt wird "ist" in das gerade erstellte dict reingepackt
%% 3. Dann wird "Test" in das dict gepackt
%% Wir machen also nacheinander für alle Listenelemente einen Aufruf 
%% von dict:append()
%%					
-spec groupBy(fun((A) -> B), list(A)) -> dict:dict(B, A).
groupBy(GBFun, List) -> 
  lists:foldl(fun(Ele, Dikke) -> 
    dict:append(GBFun(Ele), Ele, Dikke) end, dict:new(), List).

	
%%
%% dictionaryOccurences soll die Liste der Woerter laden und nach den
%% Buchstabenvorkommen indexieren. Fuer das Laden des Files kann die Funktion
%% loadDictionary/0 (am Ende der Aufgabenstellung) verwendet werden.
%% Die Gruppierung der Woerter soll ueber die vorausgehende Funktion groupBy/2
%% erfolgen. Dabei muss die Funktion letterOccurences/1 eingesetzt werden.
%% Weiterhin muessen - um Gross- und Kleinschreibung zusammenzufuehren -
%% die zu indizierenden Woerter in Kleinbuchstaben umgewandelt werden, so dass
%% bspw. die Buchstabenkombination [{$i,1,{$l,1},{$n,1}] sowohl die Woerter
%% "Lin" als auch "nil" ergibt.
%%
%% spec ist die Funktionsdeklaration (Signatur); dictionaryOccurences/0 muss 
%% keine Parameter haben und ein Erlang-dict ODER einen Fehlertupel zurückgeben
%%
%% ERKLÄRUNG: loadDictionary/0 aufrufen und die Liste von Charlisten per 
%% PatternMatching extrahieren
%% Merke: eine Liste von Charlisten bedeutet -> eine Liste von Strings
%% die Zeilenanzahl brauchen wir nicht, deshalb der Unterstrich
%% groupBy/2 erstellt das dict, in dem der Key die Tupelliste ist und
%% der/die/das (?) Value die Charliste (der String).
%% Damit der Test funzt, müssen wir die Liste einmal umdrehen
%% string:to_lower macht alles in Kleinbuchstaben vor dem Aufruf
%%	
-spec dictionaryOccurences()-> dict:dict() | {error, atom()}.
dictionaryOccurences() -> 
  {ok, {CListe, _}} = loadDictionary(),
  groupBy(fun (Wort) ->
    letterOccurences(string:to_lower(Wort)) end, lists:reverse(CListe)).


%%
%% cominations/1 soll alle moeglichen Buchstabenteilmengen, die durch die 
%% uebergebene occurrenceList gebildet werden koennen, berechnen. So soll bspw.
%% der Aufruf von combinations([{$a,2},{$b,2}]) folgende Kombinationen bilden:
%% [[{97,1}],
%% [{97,1},{98,1}],
%% [{97,1},{98,2}],
%% [{97,2}],
%% [{97,2},{98,1}],
%% [{97,2},{98,2}],
%% [{98,1}],
%% [{98,2}]]
%% Achtung: Die Anzahl der Buchstabenvorkommen (zweiter Wert des Tupels) 
%% muessen immer groesser 0 sein. 
%%
%% spec ist die Funktionsdeklaration (Signatur); removeZero/2 muss ne 
%% Tupelliste und nen Tupel als Parameter haben und ne Tupelliste zurückgeben
%%
%% ERKLÄRUNG: Wenn der Tupel ne 0 enthält, gibt den Rest zurück 
%% (der Tupel wird weggemüllt praktisch)
%% Wenn nicht, dann pack den Tupel vorne an die Tupelliste ran
%%
-spec removeZero(occurrenceList(), {char(), non_neg_integer()}) -> occurrenceList().
removeZero(Rest, {_, 0}) -> Rest;
removeZero(Rest, Tupel) -> [Tupel | Rest].


%%
%% spec ist die Funktionsdeklaration (Signatur); combinations/1 muss ne
%% Tupelliste als Parameter haben und ne Liste von Tupellisten zurückgeben
%%
%% ERKLÄRUNG: Wenn die Tupelliste leer ist, gib eine leere Liste von
%% leere Liste zurück
%% Ansonsten, nimm immer einen Tupel, und mach per List Generator eine Liste
%% von Listen mit Rekursivem Aufruf mit dem Rest der Tupelliste;
%% und mach die 0er Tupel raus
%%
-spec combinations(occurrenceList()) -> list(occurrenceList()).
combinations([]) -> [ [] ];
combinations([{Letter, Occ} | XS]) -> 
  [removeZero(Y, {Letter, Q}) || Y <- combinations(XS), Q <- lists:seq(0, Occ)].


%%
%% Subtract bekommt als Parameter zwei Listen von Buchstabenvorkommen 
%% und soll die erste von der zweiten Abziehen. So ergibt bspw. der Aufruf: 
%% subtract([{$a,3},{$b,2},{$c,5}],[{$b,7},{$a,6},{$d,8},{$c,5}])
%% das Ergebnis [{$a,3},{$b,5},{$d,8}].
%%
%% spec ist die Funktionsdeklaration (Signatur); subtract/2 muss 2 Tupellisten
%% als Parameter haben und eine Tupelliste zurückgeben 
%% (is ja logisch, Zahl minus Zahl macht ja auch Zahl)
%%
%% ERKLÄRUNG: Von innen nach außen: wir machen beide Listen zu Erlang-dicts
%% Dann mergen wir die mit merge(); wenn Buchstabe in beiden vorkommt,
%% ziehe Zahl des 1. von Zahl des 2. ab
%% Dann machen wir daraus wieder ne Liste mit to_list()
%% Darauf wenden wir filter() an, entferne alle, dessen Zahl <= 0 ist
%% Weil a5 - a6 soll ja nich a,-1 werden, sondern [] 
%% (wir ziehen die 1. von der 2. ab!!! sagt die Aufgabe! Lesen bildet!)
%% Zum Schluss sortieren wir noch nach Buchstaben von klein nach groß
%%
-spec subtract(occurrenceList(), occurrenceList()) -> occurrenceList().
subtract(Occ1, Occ2)-> 
  lists:sort(fun({C1, _}, {C2, _}) -> C1 =< C2 end,
    lists:filter(fun({_, Y}) -> Y > 0 end, 
      dict:to_list(
        dict:merge(fun(_, E1, E2) -> 
		  E2 - E1 end, dict:from_list(Occ1), dict:from_list(Occ2)
        )
      )
    )				  
  ).


%%	
%% getWordLists soll aus einer beliebigen occurenceList und einem Dictionary,
%% die Listen von Woertern bilden, die durch die occurrenceList repraesentiert
%% werden koennen. So soll bspw. der Aufruf: 
%% "getWordLists([{$e,1},{$i,1},{$l,2},{$n,1},{$r,1},{$u,2},{$x,1},{$z,1}], 
%% dictionaryOccurences())" folgende Liste von Woertern ergeben:
%% [["Zulu","Rex","nil"],
%% ["Zulu","Rex","Lin"],
%% ["Rex","Zulu","nil"],
%% ["Rex","Zulu","Lin"],
%% ["Uzi","Rex","null"],
%% ["Rex","Uzi","null"],
%% ["Zulu","nil","Rex"],
%% ["Zulu","Lin","Rex"],
%% ["Uzi","null","Rex"],
%% ["null","Uzi","Rex"],
%% ["nil","Zulu","Rex"],
%% ["Lin","Zulu","Rex"],
%% ["rulez","Linux"],
%% ["Rex","null","Uzi"],
%% ["null","Rex","Uzi"],
%% ["Linux","rulez"],
%% ["Rex","nil","Zulu"],
%% ["Rex","Lin","Zulu"],
%% ["nil","Rex","Zulu"],
%% ["Lin","Rex","Zulu"]]
%%
%% spec ist die Funktionsdeklaration (Signatur); getWordLists/2 muss ne 
%% Tupelliste und nen Dict als Parameter haben und eine Liste von Listen von 
%% Buchstabenlisten zurückgeben (siehe oben)
%%
%% ERKLÄRUNG: Wenn Liste leer ist, gib ne leere Liste von leere Liste zurück
%% Ansonsten rekursiver Listenaufbau, wobei immer eins vorne rangehängt wird:
%% Treffer kriegt erstmal die möglichen Kombination der Tupelliste. -> 
%% das sind also alle Worte, die aus dieser Tupelliste enstehen können.
%% Dann gucken wir für jede Kombination, ob sie im Dictionary enthalten ist:
%% Wenn ja, existiert also ein Wort dafür, und wir packen es in die 
%% Ergebnisliste; Wenn nicht, dann ist die Liste halt leer
%% Dann ziehen wir die Kombinationen, die wir benutzt haben, von der Tupelliste
%% ab per subtract/2; Mit dieser verkleinerten Tupelliste rufen wir dann wieder
%% getWordLists/2 auf, was ja auch Sinn macht: Wenn ein Wort benutzt wurde, 
%% kannste die Buchstaben davon nicht nochmal benutzen
%%
-spec getWordLists(occurrenceList(), dict:dict()) -> list(list(wort())).
getWordLists([], _) -> [ [] ];
getWordLists(OccListe, Dik) -> 
  [[Word|WordLists] || Treffer <- combinations(OccListe), 
    Word <- case dict:find(Treffer, Dik) of
      {ok, Gefunden} -> Gefunden;
      _ -> []
    end, 
    WordLists <- getWordLists(subtract(Treffer, OccListe), Dik)
  ].


%%
%% filterWords/2 bekommt eine Liste von Zahlen und eine Liste von Saetzen 
%% und ermittelt die Saetze, deren Buchstabenfolge sich durch die Zahlfolge
%% repraesentieren laesst (richtige Reihenfolge).
%%
%% spec ist die Funktionsdeklaration (Signatur); filterWord/2 muss ne Charliste
%% und ne Charliste als Parameter haben (also 2 Strings Man) 
%% und einen boolean zurückgeben (TRUE oder FALSE)
%%
%% ERKLÄRUNG: das hier ist ne Hilfsfunktion für filterWords/2
%% das Linke ist die Nummernliste: "23423", Rechts ist ein String "Erlang"
%% Abbruchbedingung: Wenn beide leer sind, ist die Reihenfolge richtig, 
%% denn es gibt keine :P -> TRUE
%% 2. Fall: Wenn die Längen unterschiedlich sind, kann die Reihenfolge 
%% ja nicht stimmen -> FALSE
%% Ansonsten guck die erste Ziffer und den ersten Buchstaben an: 
%% Wandle den Buchstaben mit der Hilfsfunktion in die Ziffer um 
%% und vergleiche beide Ziffern: Wenn sie gleich sind, prüfe den Rest 
%% (also 2. Buchstaben mit 2. Ziffer etc)
%% Wenn nicht, kannste schon abbrechen
%%
-spec filterWord(wort(), wort()) -> boolean().
filterWord([], []) -> true;
filterWord(Zif, Buch) when length(Zif) /= length(Buch) -> false;
filterWord([N | NR], [B | BR]) ->
  case N == assignNum(B) of
    true -> filterWord(NR, BR);
    false -> false
  end.

	
%%
%% spec ist die Funktionsdeklaration (Signatur); filterWords/2 muss ne 
%% Charliste und ne Liste von Charlisten als Parameter haben 
%% und eine Liste von CharListen zurückgeben
%%
%% ERKLÄRUNG: Die SatzList enthält eine Liste von SÄTZEN 
%% d.h. sie enthält Listen von Strings (["das", "ist", "ein", "satz"])
%% Wir wollen jetzt alle Stringlisten herausfiltern, die passend sind
%% Dazu nehmen wir uns also immer eine Stringliste (einen "Satz") 
%% und machen den in einen String mit string:join()
%% Der Satz wäre z.B. also: "dasisteinsatz"; damit wird dann filterWord/2
%% aufgerufen (die Hilfsfunktion); immer wenn die TRUE zurückgibt, 
%% wird der Satz in die Ergebnisliste übernommen
%% Um Groß- und Kleinbuchstaben brauchen wir uns nicht kümmern, 
%% weil assignNum/1 das schon macht
%%
-spec filterWords(wort(), list(list(wort()))) -> list(wort()).
filterWords(NumList, SatzList) -> 
  lists:filter(fun(Satz) -> 
    filterWord(NumList, string:join(Satz, "")) end, SatzList).

	
%%
%% getSentences fuegt die einzelnen bisher geschriebenen Funktionen zusammen.
%% getSentences bekommt eine Nummernliste und erzeugt daraus die 
%% Buchstabenkombinationen, die sich daraus bilden lassen. Aus den 
%% Buchstabenkombis werden dann die Saetze ermittelt, die sich bilden lassen.
%%
%% spec ist die Funktionsdeklaration (Signatur); getSentences/1 muss ne 
%% Charliste als Parameter haben und ne Liste von Listen von Buchstabenlisten
%% zurückgeben; also z.B. [["df", "asd", "sdf"], ["sdf", "dsf", "wer"]]
%%
%% ERKLÄRUNG: war ja schon alles da:
%% extractLetters/1 gibt die möglichen Worte zurück, also bei 23 z.B.:
%% "da","db","dc","ea","eb","ec","fa","fb","fc"
%% Dann wird für jedes dieser Worte die OccurenceList gebaut (die Tupelliste)
%% dann wird das Dict gebaut 
%% Dann wird getWordLists/2 auf jedes Wort 
%% (bzw. auf die OccurenceList jedes Wortes) angewandt
%% Wir erhalten also eine Liste von Stringlisten: [["Zulu","Rex","nil"], ...]
%%
-spec getSentences(wort()) -> list(list(wort())).  
getSentences(NumberList) ->
  PossWords = extractLetters(NumberList),
  OccListWords = lists:map(fun(X) -> letterOccurences(X) end, PossWords),
  Dict = dictionaryOccurences(),
  lists:flatmap(fun(X) -> getWordLists(X, Dict) end, OccListWords).


%% ============================================================================
%% VORGEGEBENE HILFSFUNKTIONEN
%% ============================================================================
%%
%% Load Words from Dictionary
%% loadDictionary laedt das Woerterbuch in eine Liste von Strings.
%% Achtung: Das Woerterbuch ist ueber die Linux-Manpages generiert - manche 
%% Woerter ergeben nicht unbedingt augenscheinlichen Sinn. 
%%
%% spec ist die Funktionsdeklaration (Signatur); frname/0 muss keine Parameter
%% haben und eine Liste von Chars zurückgeben (den Dateinamen praktisch)
%%
-spec frname() -> wort().		
frname() -> "words_eng.txt".


%%
%% spec ist die Funktionsdeklaration (Signatur); loadDictionary/0 muss keine 
%% Parameter haben und eine Tupel zurückgeben, was entweder 
%% {ok, Tupel aus Liste von Charlisten und nem Int*} ist ODER {error, atom}
%%
%% ERKLÄRUNG: das hier öffnet und schließt die Textdatei  
%% und ruft die Einlesen-Funktion auf
%% *(dieser Int ist die Zeilenanzahl btw)
%%
-spec loadDictionary() -> {ok, {list(wort()), integer()}} | {error, atom()}.
loadDictionary() ->    
  case file:open(frname(), [read]) of
    {'ok', S} ->  Content = reader(S, 0, []),
      file:close(S), {ok, Content};
    {'error', Why} -> {error, Why}
  end.

  
%%
%% spec ist die Funktionsdeklaration (Signatur); reader/3 muss 3 Params haben 
%% und ein Tupel zurückgeben, der aus Listen von Strings und Integern besteht
%%
%% ERKLÄRUNG: das hier "liest" praktisch die Datei aus
%%
-spec reader(any(),integer(),list(wort()))-> {list(wort()),integer()}.
reader (File, N, Akku) ->
  case io:get_line(File, '') of 
    eof -> {lists:reverse(Akku), N};
    {error, Reason} -> Reason;
    Line -> 
	  reader(File, N + 1, [lists:filter(fun(X) -> X /= $\n end, Line) | Akku])
  end.

  
%%
%% spec ist die Funktionsdeklaration (Signatur); assignChar/1 muss nen Char 
%% als Parameter haben und eine Liste von Chars zurückgeben
%%
%% ERKLÄRUNG: das ist halt dieses Telefonpad-Dingens; aus Zahl mach Buchstaben
%%
-spec assignChar(char()) -> wort().
assignChar($2) -> [$a, $b, $c];
assignChar($3) -> [$d, $e, $f];
assignChar($4) -> [$g, $h, $i];
assignChar($5) -> [$j, $k, $l];
assignChar($6) -> [$m, $n, $o];
assignChar($7) -> [$p, $q, $r, $s];
assignChar($8) -> [$t, $u, $v];
assignChar($9) -> [$w, $x, $y, $z].


%%
%% spec ist die Funktionsdeklaration (Signatur); assignNum/1 muss nen Char  
%% als Parameter haben und einen Char zurückgeben
%%
%% ERKLÄRUNG: das ist praktisch assignChar/1() andersrum, 
%% also aus Buchstaben mach Zahl
%%
-spec assignNum(char()) -> char().
assignNum(X) when X==$a; X==$b; X==$c; X==$A; X==$B; X==$C -> $2;
assignNum(X) when X==$d; X==$e; X==$f; X==$D; X==$E; X==$F -> $3;
assignNum(X) when X==$g; X==$h; X==$i; X==$G; X==$H; X==$I -> $4;
assignNum(X) when X==$j; X==$k; X==$l; X==$J; X==$K; X==$L -> $5;
assignNum(X) when X==$m; X==$n; X==$o; X==$M; X==$N; X==$O -> $6;
assignNum(X) when X==$p; X==$q; X==$r; X==$s; X==$P; X==$Q; X==$R; X==$S -> $7;
assignNum(X) when X==$t; X==$u; X==$v; X==$T; X==$U; X==$V -> $8;
assignNum(X) when X==$w; X==$x; X==$y; X==$z; X==$W; X==$X; X==$Y; X==$Z -> $9.