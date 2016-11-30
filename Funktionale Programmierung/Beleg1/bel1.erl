%% coding: utf-8
%% @author CHRIS
%% @doc Belegaufgabe 1

-module(bel1).
-compile(export_all). % alle Methode "public" machen
%-export([encode/2, decode/2, createCodeTree/1]). % nur die genannten Methoden "public" machen

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ein Huffman Code wird durch einen Binaermaum repraesentiert.
% Typo fixed: "Binaermaum" -> "Binaerbaum"
%
%  Jedes Blatt beinhaltet ein Zeichen, das durch den Baum kodiert wird.
%  Das Gewicht entspricht der Haeufigkeit des Vorkommens eines Zeichen innerhalb eines Texts.
%    
%  Die inneren Knoten repraesentieren die Kodierung. Die assoziierten Zeichen weisen auf die 
%  darunter liegenden Blaetter. Das Gewicht entspricht der Summe aller Zeichen, die darunter liegen.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% DEFINITIONEN
%
% Definition of the Tree: two kinds of nodes:
% fork - representing the inner nodes (binary tree)
% leaf - representing the leafs of the tree
% der Tree ist halt entweder ein Blatt (Baum ist dann nur ein Buchstabe)
% oder ein Knoten (halt also zwei Teilbäume -> also die Wurzel)
%
-type tree():: fork() | leaf().

% record beschreibt die Struktur für einen innere Knoten (fork); hat linken und rechten Teilbaum,
% eine Liste von chars und ein Gewicht (Integer größer gleich 0)
%
% type definiert eine Typdefinition für einen inneren Knoten -> so wie in record definiert, referenziert von #
-record(fork, {left::tree(), right::tree(), chars::list(char()), weight::non_neg_integer()}).
-type fork() :: #fork{}.

% record beschreibt die Struktur für ein Blatt (leaf); hat einen Buchstaben (char) 
% und ein Gewicht (Integer größer gleich 0)
%
% type definiert eine Typdefinition für ein Blatt -> so wie in record definiert, referenziert von #
-record(leaf, {char::char(), weight::non_neg_integer()}).
-type leaf() :: #leaf{}.

% Ein Bit ist 1 ODER 0
-type bit() :: 0 | 1. 

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% BASISFUNKTIONEN
%
% Rückgabe des Gewichts eines Blatts / Knotens
% spec ist die Funktionsdeklaration (Signatur); weight/1() muss nen tree() als Parameter haben 
% und einen Integer größer gleich 0 zurückgeben (das Gewicht)
-spec weight(tree()) -> non_neg_integer(). 
weight(#fork{weight=W}) -> W;
weight(#leaf{weight=W}) -> W.

% Rückgabe einer Liste von chars, die einen Knoten repräsentieren
% spec ist die Funktionsdeklaration (Signatur); chars/1() muss nen tree() als Parameter haben 
% und eine Liste von chars zurückgeben
-spec chars(tree()) -> list(char()). 
chars(#fork{chars=C}) -> C; % innerer Knoten, C ist also schon eine Liste von chars und wird zurückgegeben
chars(#leaf{char=C}) -> [C]. % Blatt, C ist also nur ein Char, also machen wir den in eine Liste rein

% Erzeugung eines CodeTrees aus zwei Teilbaeumen
% Aus Gruenden der Testbarkeit werden links die Teilbaeume mit dem alphabetisch kleinerem Wert der 
% Zeichenketten angeordnet. 
%
% spec ist die Funktionsdeklaration (Signatur); makeCodeTree/2() muss zwei Bäume als Parameter haben 
% und einen Baum zurückgeben; es werden also immer 2 Bäume zu einem gemacht
%
% ERKLÄRUNG: case Abfrage: Wenn die Zeichenkette des linken Baums alphabetisch kleiner ist als die des rechten
%			 (so wie es in der Aufgabe gefordert war), dann erstelle einen neuen Knoten (fork) mit:
%			 linker Baum ist T1, rechter Baum ist T2, die Zeichenkette wird die Konkatenation von 
%			 den Zeichenketten von T1 und T2 und das Gewicht ist einfach die Summe der Kindgewichte
%			 Wenn nicht, dann wird ebenfalls ein neuer Knoten erstellt (fork), aber diesmal mit
%			 "umgedrehten" Werten -> links T2, rechts T1, Zeichenkette andersrum konkatenieren, Gewicht gleich
% 
% war nicht in der Datei bel1.erl enthalten, aber in der pdf aufgeschrieben
% (wurde also einfach kopiert)
-spec makeCodeTree( T1::tree(), T2::tree()) -> tree().
makeCodeTree(T1 , T2) -> case (chars(T1) < chars(T2)) of % Erlang vergleicht lexikographisch, d.h nur die ersten beiden Buchstaben, wenn die gleich sind, die nächsten beiden usw...

	true -> #fork{left = T1, right = T2, chars = chars(T1) ++ chars(T2), weight = weight(T1) + weight(T2)};		
	false -> #fork{left = T2, right = T1, chars = chars(T2) ++ chars(T1), weight = weight(T1) + weight(T2)}
end.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    Erzeugung eines Huffman Trees
%
%   AUFGABE 1: Schreiben Sie eine Funktion createFrequencies, die aus einem Text die Haeufigkeiten des Vorkommens
%   eines Zeichen in der Zeichenkette berechnet.
% 
%  Ergebnis der Funktion soll eine Liste von Zweiertupeln sein, die als erstes Element den Character und als 
%  zweites die Haeufigkeit enthaelt.
%
%  createFrequencies("Dies ist ein Test") waere also [{$D,1}, {$i,3}, {$e,3}, {$s, 3}, {$ , 3}, {$t, 2}, {$n, 1}, {$T,1}] 
%  
%  Auf die Elemente eines Tupels kann ueber Pattern Matching zugegriffen werden: 
%  z.B. {X,Y} = {$a,4}
%  Tipp: Splitten Sie die Funktion auf:
%  1. Funktion zum Eingliedern des Buchstabens in die Tupelliste (z.B. addLetter(...))
%  2. Aufruf der Funktion fuer jeden Buchstaben

%
% spec ist die Funktionsdeklaration (Signatur); addLetter/2() muss eine Liste von Tupeln und einen Buchstaben (char)
% als Parameter bekommen und eine Liste von Zweiertupeln zurückgeben
%
% ERKLÄRUNG: Tupels ist die Eingabeliste (am Anfang leer), Buchstabe ist der Buchstabe, der eingefügt wird
% 1. Akkumulator erstellen -> das ist die leere Liste (wir gehen zu addLetter/3)
% 2. Der 1. Fall ist die Abbruchbedingung -> wenn die Eingabeliste leer ist, wird eine Liste zurückgeben
% Diese enthält den Akkumulator + einen neuen Zweiertupel (Buchstabe, 1)
% 3. Der 2. Fall ist Pattern Matching; das Pattern matcht nur dann, wenn addLetter() mit einem Buchstaben
% aufgerufen wurde, der schon in der Tupelliste enthalten war.
% Dann wird eine neue Liste zurückgegeben, die den Akkumulator enthält (Akku) + eine neuen Tupel für
% den Buchstaben B mit einer um 1 erhöhten Häufigkeit und der Rest der Tupelliste
% 4. Der 3. Fall ist die Rekursion: der 1. Tupel der Eingangsliste wird hinten in den Akkumulator gepackt,
% und der Rest der Eingangsliste wird die neue Tupelliste
%
% ALSO: Wenn die Tupelliste leer ist -> neuen Tupel anlegen mit Häufigkeit 1 (logisch, ist ja das 1. Auftreten)
% Wenn erster Tupel der Tupelliste für B matcht, erhöhe dessen Auftreten um 1 und mach ne Liste draus
% (also der alte Tupel für B wird praktisch entfernt, und vorne in aktualisierter Form wieder angehängt)
% Wenn erster Tupel der Tupelliste nicht für B matcht, packe ihn in den Akkumulator und prüfe, ob der jetzt
% erste Tupel (also der 2. insgesamt) für B matcht. Das wird dann so lange wiederholt, bis 
% A) die Tupelliste durchgearbeitet wurde (also leer ist) -> dann mache einen neuen Tupel für B (1.Fall)
% oder B) ein matchender Tupel für B gefunden wurde -> dann erhöhe dess Auftreten (2. Fall)
%
-spec addLetter(list({char(),non_neg_integer()}), char())-> list({char(), non_neg_integer()}).
addLetter(Tupels, Buchstabe) -> addLetter(Tupels, Buchstabe, []).

-spec addLetter(list({char(),non_neg_integer()}), char(), list({char(),non_neg_integer()}))-> list({char(), non_neg_integer()}).
addLetter([], B, Akku) -> Akku ++ [{B, 1}];
addLetter([T| TS], B, Akku) -> 
	case T of 
		{B, W} -> Akku ++ [{B, W + 1}| TS];
		_ -> addLetter(TS, B, Akku ++ [T])
	end.

%
% spec ist die Funktionsdeklaration (Signatur); createFrequencies/1() muss eine Liste von Buchstaben (chars) als
% Parameter bekommen und eine Liste von Zweiertupeln zurückgeben
%
% ERKLÄRUNG: "Text" ist das was reinkommt, "Neu" ist die Liste, die am Ende rauskommen soll.
% 0. Akkumulator erstellen -> leere Liste (wir gehen in createFrequencies/2)
% 1. Nimm den 1. Buchstaben aus der Liste "Text" (C) und rufe die Methode addLetter() damit auf
% 2. addLetter() gibt dann eine Liste zurück mit einem Tupel -> [{$t,1}]
% 3. Dann wird createFrequencies() rekursiv aufgerufen; die Eingabeliste ist jetzt nicht mehr "Text",
% sondern "Text ohne den 1. Buchstaben" (CS) und Neu ist jetzt nicht mehr leer, sondern enthält ein Tupel
% 4. Es wird wieder der 1. Buchstabe (der 2. des Textes) genommen, und addLetter() damit aufgerufen
% 5. 1. Fall ist die Abbruchbedingung: TextListe ist leer, also gib "Neu" zurück
%
-spec createFrequencies(list(char())) -> list({char(), non_neg_integer()}).
createFrequencies(Text) -> createFrequencies(Text, []).

-spec createFrequencies(list(char()), list({char(), non_neg_integer()})) -> list({char(), non_neg_integer()}).
createFrequencies([], Neu) -> Neu;
createFrequencies([B| BS], Neu) -> Tupels = addLetter(Neu, B), createFrequencies(BS, Tupels).

%
% spec ist die Funktionsdeklaration (Signatur); sortNodesWeightFirstCharSec/2() muss 2 tree() Objekte
% als Parameter bekommen und einen boolean() zurückgeben
%
% ERKLÄRUNG: Hilfsfunktion für makeOrderedLeafList() und combine() zum Sortieren
% Es gibt 4 Fallunterscheidungen; für alle Kombination von 2 tree() Objekten:
% leaf/leaf, leaf/fork, fork/leaf und fork/fork
% 
% Laut Erlärungstext für combine() sollen die Knoten so eingefügt werden, so dass ein Baum wie auf dem
% Aufgabenblatt entsteht. Es ergibt sich also folgendes Kriterium an die Sortierung:
% - es soll aufsteigend nach Gewicht sortiert werden.
% Also für den Testbaum auf dem Aufgabenblatt würde sich ergeben: (nach makeOrderedLeafList())
% [{leaf,$c,1},{leaf,$e,1},{leaf,$g,1},{leaf,$d,1},{leaf,$h,1},{leaf,$f,1},{leaf,$b,3},{leaf,$a,8}]
% -> Beachte: bei gleichen Gewichten wird nichts weiter unternommen! Das führt aber dazu, dass bei
% combine() die "falschen" Knoten zusammengepackt werden, z.b. "C" und "E" zuerst oder "D" und "G".
% Laut dem Aufgabenblatt soll aber folgendes passieren:
% G und H zusammenpacken, dann E und F, dann C und D, dann GH und EF, dann CD und B
% Um das zu erreichen, muss also noch nach einem 2. Kriterium sortiert werden:
% - wenn das Gewicht gleich ist (dann, und NUR DANN), sortiere nach Buchstaben ABSTEIGEND!!!
% makeOrderedLeafList() führt dann zu: [{leaf,$h,1},{leaf,$g,1},{leaf,$f,1},{leaf,$e,1},{leaf,$d,1},{leaf,$c,1},{leaf,$b,3},{leaf,$a,8}]
% Deshalb also eine Hilfsfunktion, die lists:sort übergeben werden kann
% 
% Funktionsweise: Erlang vergleicht Tupel immer Elementenweise nacheinander
% Hier wird also erst L1 < R1 getestet; wenn die gleich sind, wird R2 < L2 getestet
% R2 und L2 sind vertauscht, um das ABSTEIGEND zu erreichen
% 
-spec sortNodesWeightFirstCharSec(tree(), tree()) -> boolean(). 
sortNodesWeightFirstCharSec(#leaf{weight = L1, char = L2}, #leaf{weight = R1, char = R2}) -> {L1, R2} < {R1, L2};
sortNodesWeightFirstCharSec(#leaf{weight = L1, char = L2}, #fork{weight = R1, chars = R2}) -> {L1, R2} < {R1, L2};
sortNodesWeightFirstCharSec(#fork{weight = L1, chars = L2}, #leaf{weight = R1, char = R2}) -> {L1, R2} < {R1, L2};
sortNodesWeightFirstCharSec(#fork{weight = L1, chars = L2}, #fork{weight = R1, chars = R2}) -> {L1, R2} < {R1, L2}.

%  Erzeugung eines Blattknotens fuer jeden Buchstaben in der Liste
%  Aufsteigendes Sortieren der Blattknoten nach den Haeufigkeiten der Vorkommen der Buchstaben
%  z.B. aus makeOrderedLeafList([{$b,5},{$d,2},{$e,11},{$a,7}])
% wird [#leaf{char=$d,weight=2},#leaf{char=$b,weight=5},#leaf{char=$a,weight=7},#leaf{char=$e,weight=11}]
%
% Typo fixed: "FegList" -> "FreqList"
%
% spec ist die Funktionsdeklaration (Signatur); makeOrderedLeafList/1() muss eine Liste von Zweiertupeln als 
% Parameter bekommen und eine Liste von Blättern (leafs) zurückgeben
%
% ERKLÄRUNG: List Comprehension; für jedes Tupel {C,I} aus Freqlist wird ein Blatt erzeugt und in eine Liste gepackt
% Aufsteigend Sortieren, also lists:sort() mit einer Funktion orderLeafs(), die das Gewicht von 2 leafs vergleicht
%
-spec makeOrderedLeafList(FreqList::list({char(), non_neg_integer()})) -> list(leaf()).
makeOrderedLeafList(FreqList) -> lists:sort(fun sortNodesWeightFirstCharSec/2, [#leaf{char = B, weight = I} || {B, I} <- FreqList]).

%  Bei jedem Aufruf von combine sollen immer zwei Teilbaeume (egal ob fork oder leaf) zusammenfuegt werden.
%  Der Parameter der Funktion combine ist eine aufsteigend sortierte Liste von Knoten.
%
%  Die Funktion soll die ersten beiden Elemente der Liste nehmen, die Baeume zusammenfuegen
%  und den neuen Knoten wieder in die Liste einfuegen sowie die zusammengefuegten aus der Liste 
%  loeschen. Dabei sollen der neue Knoten so eingefuegt werden, dass wieder eine sortierte Liste von
%  Knoten entsteht.
%
%  Ergebnis der Funktion soll wiederum eine sortierte Liste von Knoten sein.
% 
%  Hat die Funktion weniger als zwei Elemente, so soll die Liste unveraendert bleiben.
%  Achtung: Ob die vorgefertigten Tests funktionieren, haengt davon ab, auf welcher Seite die Knoten
%  eingefuegt werden. Die Tests sind genau dann erfolgreich, wenn Sie die Baeume so kombinieren, dass 
%  ein Baum entsteht, der so angeordnet ist, wie im Beispiel auf dem Aufgabenblatt. Sorgen Sie dafuer,
%  dass die Teilbaeume ebenso eingefuegt werden (erhoehter Schwierigkeitsgrad) oder schreiben Sie eigene
%  Tests. 
%
% spec ist die Funktionsdeklaration (Signatur); combine/1() muss eine Liste von tree() Objekten 
% (können also fork() oder leaf() sein) als Parameter bekommen und eine Liste von tree() Objekten
% zurückgeben, die dann um 1 kürzer ist, weil ja 2 Elemente zusammengefügt wurden
%
% ERKLÄRUNG: 1. Fall: Leere Liste -> nichts machen, leere Liste einfach wieder zurück
% 2. Fall: [Baums] matcht auf Liste mit einem Element, nichts tun (siehe Aufgabenstellung)
% 3.Fall: 2 oder mehr Elemente, also -> rufe die Funktion makeCodeTree() mit den ersten beiden
% Listenelementen auf und füge das enstandene Element in die Liste ein (der Rest bleibt gleich)
% Damit sind die "alten" beiden Elemente "gelöscht" und durch das neue "ersetzt" worden (Aufgabenstellung)
% Für die gesamte Liste muss dann noch lists:sort() aufgerufen werden, um das ganze zu sortieren
% Eine Custom Funktion ist mit angeben, um nach best. Kriterien zu sortieren (siehe oben)
% 
-spec combine(list(tree())) -> list(tree()).
combine([]) -> [];	
combine([Baums]) -> [Baums];
combine([A, B|BS]) -> lists:sort(fun sortNodesWeightFirstCharSec/2, [makeCodeTree(A, B)|BS]). 

%  Die Funktion repeatCombine soll die Funktion combine so lange aufrufen, bis nur noch ein Gesamtbaum uebrig ist.		
%
% spec ist die Funktionsdeklaration (Signatur); repeatCombine/1() muss eine Liste von tree() Objekten als Parameter
% bekommen und einen einzigen tree() zurückgeben
%
% ERKLÄRUNG: 1. Fall: [Treelist] matcht nur dann, wenn die Liste ein Element enthält -> finaler Tree, also zurückgeben
% 2. Fall: mehr als 1 Element, also rekursiv aufrufen mit einer neuen TreeListe, die einen weniger hat
% -> combine(TreeList) hat ja aus X Elementen X-1 Elemente gemacht
%
-spec repeatCombine(TreeList::list(tree())) -> tree().
repeatCombine([TreeList])-> TreeList;
repeatCombine(TreeList) -> repeatCombine(combine(TreeList)).

%  createCodeTree fuegt die einzelnen Teilfunktionen zusammen. Soll aus einem gegebenen Text, den Gesamtbaum erzeugen.
%
% spec ist die Funktionsdeklaration (Signatur); createCodeTree/1() muss eine Liste von chars() als Parameter
% bekommen (ein String also) und einen tree() zurückgeben (der Huffman Baum)
%
% ERKLÄRUNG: createFrequencies(Text) erzuegt aus der CharListe eine ZweiertupelListe
% makeOrderedLeafList() macht daraus dann eine geordnete leaf-Liste (Liste von Blättern)
% repeatCombine)() fügt dann solange immer 2 Elemente zusammen, bis zum finalen tree()
%
-spec createCodeTree(Text::list(char())) -> tree().
createCodeTree(Text)-> Tupelz = createFrequencies(Text), 
					   Blattliste = makeOrderedLeafList(Tupelz),
					   repeatCombine(Blattliste).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% Dekodieren einer Bitsequenz
%
% Die Funktion decode soll eine Liste von Bits mit einem gegebenen Huffman Code (CodeTree) dekodieren.
% Ergebnis soll die Zeichenkette im Klartext sein.	
%
% spec ist die Funktionsdeklaration (Signatur); decode/2() muss einen tree() und eine Liste von bit()
% als Parameter bekommen und eine Liste von char() zurückgeben (also einen String letztendlich)
%
% ERKLÄRUNG: 1. Fall: tree() ist fork() und BitListe beginnt mit 0 -> nach Links gehen und damit weitermachen
% (d.h.: Linker Knoten ist neuer tree() und der Rest der BitListe ist die neue BitListe)
% 2. Fall: tree() ist fork() und BitListe beginnt mit 1 -> nach Rechts gehen und damit weitermachen
% (d.h.: Rechter Knoten ist neuer tree() und der Rest der BitListe ist die neue BitListe))
% 3. Fall: tree() ist ein leaf() und BitListe ist leer -> Buchstabe des leaf() zurückgeben
% (ohne Bitsequenz kann man ja nichts mehr dekodieren)
% 4. Fall: tree() ist ein leaf() und BitListe nicht leer -> Buchstabe an Listenanfang und weitermachen
%
-spec decode(CodeTree::tree(), list( bit())) -> list(char()).
decode(CodeTree, BitList) -> decode(CodeTree, CodeTree, BitList).

-spec decode(tree(), tree(), list( bit())) -> list(char()).
decode(#fork{left = L}, Akku, [0|BitList]) -> decode(L, Akku, BitList);
decode(#fork{right = R}, Akku, [1|BitList]) -> decode(R, Akku, BitList);
decode(#leaf{char = B},_,[]) -> [B];
decode(#leaf{char = B}, Akku, BitList) -> [B|decode(Akku, Akku, BitList)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%  Kodieren einer Bitsequenz
%
%  Die Funktion encode soll eine Liste von Bits mit einem gegebenen Huffman Code (CodeTree) kodieren.
%  Ergebnis soll die Bitsequenz sein.
%
%  Gehen Sie dabei folgendermassen vor:
%  Schreiben Sie eine Funktion convert, die aus einem Codetree eine Tabelle generiert, die fuer jeden 
%  Buchstaben die jeweilige Bitsequenz bereitstellt. Dabei soll jeder Eintrag ein Tupel sein bestehend
%  aus dem Character und der Bitsequenz.
%  Also: convert(CodeTree)->[{Char,BitSeq},...]
%
% spec ist die Funktionsdeklaration (Signatur); convert/1() muss einen tree() als Parameter bekommen
% und eine ZweiertupelListe zurückgeben (immer Buchstabe + dessen Bitsequenz)
% 
% ERKLÄRUNG: 1. Fall: tree() ist fork() -> Liste aus 2 TeilListen erzeugen:
% Linke Teilliste ensteht durch rekursiven Aufruf von convert() -> linker Knoten ist neuer tree()
% und an die Sequenzliste wird eine 0 angehängt (wir sind ja links abgebogen)
% Rechte Teilliste ensteht durch rekursiven Aufruf von convert() -> rechter Knoten ist neuer tree()
% und and die Sequenzliste wird eine 1 angehängt (wir sind ja rechts abgebogen)
% 2. Fall: tree() ist leaf() -> Liste mit einem Tupel {Buchstabe, Sequenz} (Abbruchbedingung)
%
-spec convert(CodeTree::tree()) -> list({char(), list(bit())}).
convert(CodeTree) -> convert(CodeTree, []).

-spec convert(tree(), list(bit())) -> list({char(), list(bit())}).
convert(#fork{left = L, right = R}, Bitakku) -> convert(L, Bitakku ++ [0]) ++ convert(R, Bitakku ++ [1]);
convert(#leaf{char = B}, Bitakku) -> [{B, Bitakku}].

%  Schreiben Sie eine Funktion encode, die aus einem Text und einem CodeTree die entsprechende 
%  Bitsequenz generiert.
%  Verwenden Sie dabei die erzeugte Tabelle.
%
% spec ist die Funktionsdeklaration (Signatur); encode/2() muss eine Liste von char() und ein tree() als
% Parameter bekommen und eine Liste von bit() zurückgeben
% 
% ERKLÄRUNG: 1. Fall: zu Anfang: convert() erzeugt die Tabelle mit den Tupeln {Buchstabe, Sequenz} 
% Akku speichert die vollständige Bitsequenztabelle, damit die immer wieder vollständig existiert
% 2. Fall: Wenn die Liste von char() leer ist, gib eine leere Liste zurück (Abbruchbedingung)
% (Einen leeren String "" brauche ich ja nich kodieren, es gibt ja nichts zu tun)
% 3. Fall Pattern Matching: Der erste Buchstabe der Textliste entspricht dem 1. Buchstaben der Tabelle
% Der Anfang des Codes wurde also gefunden. Die Funktion encode() wird rekursiv für den Rest der Textliste aufgerufen
% 4. Fall Standard: Das erste Tupel der Tabelle hat nicht gematcht, also wird die Funktion encode() rekursiv aufgerufen
% mit dem Rest der Tupelliste
%
-spec encode(Text::list(char()), CodeTree::tree()) -> list(bit()).
encode(Text, CodeTree) -> encode(Text, convert(CodeTree), convert(CodeTree)).

-spec encode(list(char()), list({char(), list(bit())}), list({char(), list(bit())})) -> list(bit()).
encode([],_,_) -> [];
encode([Z| ZS], Table, Akku) ->
	case Table of
		[{Z, BS}| _] -> BS ++ encode(ZS, Akku, Akku);
		[_| CS] -> encode([Z| ZS], CS, Akku);
		[] -> erlang:error(es_wurden_fehler_gemacht)
	end.





