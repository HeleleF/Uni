%PosRows = 	lists:filter(fun(Element) -> lists:sum(Element) == MagicNumber end, Combs), lists:filter(fun(Element) -> length(lists:usort(Element)) == Size end, PosRows).
						


-module(b3t).
-compile(export_all). % alle Methode "public" machen


-type posint() :: non_neg_integer().
						
-spec row(posint(), posint(), list(posint())) -> list(list(posint())).
row(Max, Value, Elements) -> row(Max, Max, Value, Elements). 
row(0, _, _, _) -> [[]];
row(Max, MaxGes , Value, Elements) ->  
	[Zeil ++ [E] || 
		Zeil <- row(Max - 1, MaxGes, Value, Elements),
		E <- Elements -- Zeil, print(Zeil, E), 
		(Max /= MaxGes) or ((lists:sum(Zeil ++ [E])) == Value)].
		
		
print (P,Q)-> io:write(P), io:write(Q), io:fwrite("~n"), true.
print (P)-> io:write(P), io:fwrite("~n").
%io:fwrite(atom_to_list(lists:sum(P++[Q])<7)), io:fwrite("~n"), true.

beispiel(0) -> [[]];
beispiel(X) -> [ Y++[Q] || Y<-beispiel(X-1),Q<-lists:seq(1,4), print(Y,Q),
 lists:sum(Y++[Q])<7].
 
 
istMagisch(Quad, Max, Val) ->
Spalten = transpose(Quad),
KorrekteSpalten = lists:takewhile(fun(X) -> lists:sum(X) == Val end, Spalten),
case length(KorrekteSpalten) == Max of
  true -> checkDiaLR(Quad, Max, Val) and checkDiaRL(Quad, Max, Val);
  false -> false
end.

checkDiaLR(Q, M, V) -> QFlach = lists:flatten(Q),
  V == lists:foldl(fun(Z, Dia) -> Dia + lists:nth((M * (Z - 1) + Z), QFlach) end, 0, lists:seq(1, M)).
checkDiaRL(Q, M, V) -> QFlach = lists:flatten(Q),
  V == lists:foldl(fun(Z, Dia) -> Dia + lists:nth(((M - 1) * Z + 1), QFlach) end, 0, lists:seq(1, M)).
  
  
transpose([[]|_]) -> [];
transpose(Quadrat) ->
  [lists:map(fun hd/1, Quadrat) | transpose(lists:map(fun tl/1, Quadrat))].