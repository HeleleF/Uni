%%% coding: utf-8
%%% @author Gärtner
%%% @doc Tests Belegaufgabe 3
-module(b3_tests).
-include_lib("eunit/include/eunit.hrl").


%% ============================================================================
%% Test für row/3
%% ============================================================================

row_test() ->
  Rows3 = b3:row(3, 15, lists:seq(1, 9)),
  Rows4 = b3:row(4, 34, lists:seq(1, 16)),
  ?assert(length(Rows3) == 48),
  ?assert(length(lists:filter(fun(X) -> lists:sum(X) /= 15 end, Rows3)) == 0),
  ?assert(length(Rows4) == 2064),
  ?assert(length(lists:filter(fun(X) -> lists:sum(X) /= 34 end, Rows4)) == 0).


%% ============================================================================
%% Test für duplicate/2
%% ============================================================================
  
duplicate_test_() ->
  [
    ?_assertEqual(false, b3:duplicate([], [])),
    ?_assertEqual(false, b3:duplicate([1, 2, 3, 4, 5], [])),
    ?_assertEqual(false, b3:duplicate([], [1, 2, 3, 4, 5])),
    ?_assertEqual(false, b3:duplicate([1, 2, 3, 4, 5], [6,7, 8, 9, 10])),
    ?_assertEqual(true, b3:duplicate([1, 2, 3, 4, 5], [6, 7, 8, 9, 10, 1]))	
  ].


%% ============================================================================
%% Test für combineRows/4, max 10 Sekunden
%% ============================================================================
  
combineRows_test_() -> {timeout, 10, fun combineRows_testFun/0}.	
combineRows_testFun() -> 
  Rows3 = b3:combineRows(2, 3, 15, lists:seq(1, 9)),
  Rows4 = b3:combineRows(2, 4, 34, lists:seq(1, 16)),
  ?assert(length(Rows3) == 432),
  ?assert(length(lists:filter(fun(X) -> lists:sum(X) /= 30 end, Rows3)) == 0),
  ?assert(length(Rows4) == 1238400),
  ?assert(length(lists:filter(fun(X) -> lists:sum(X) /= 68 end, Rows4)) == 0).


%% ============================================================================
%% Test für calcSquares/3, max 10 Sekunden
%% ============================================================================
  
calcSquares_test_() -> {timeout, 10, fun calcSquares_testFun/0}.	
calcSquares_testFun() ->
  [
    ?assertEqual([[4, 9, 2, 3, 5, 7, 8, 1, 6]], 
	  b3:calcSquares([4, 9, 2], 3, 15)),
    ?assertEqual([[4, 9, 2, 3, 5, 7, 8, 1, 6]], 
	  b3:calcSquares([4, 9, 2, 3, 5, 7], 3, 15)),
    ?assertEqual([[3, 9, 8, 14, 12, 16, 5, 1, 6, 2, 11, 15, 13, 7, 10, 4]], 
	  b3:calcSquares([3, 9, 8, 14, 12, 16, 5, 1], 4, 34)),
    ?assertEqual([[15, 16, 2, 1, 3, 4, 14, 13, 6, 9, 7, 12, 10, 5, 11, 8]], 
	  b3:calcSquares([15, 16, 2, 1], 4, 34))
  ].
