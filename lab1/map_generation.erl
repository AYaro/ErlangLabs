-module (map_generation).
-export ([start/1]).

factors(N) -> factors(N, 1, 0).
factors(N, F, Count) when ((N rem F) == 0) ->
  Lim =  math:ceil(math:sqrt(N)),
  if
    (F >= Lim ) -> Count;
    true -> factors(N, F+1, Count + 2)
  end;
factors(N, F, Count) ->   factors(N, F+1, Count).

start(Lim) ->
  StartingList = lists:seq(2, 13000),
  MappedTuple = lists:mapfoldl(fun(X, Acc) -> {X+Acc, Acc+X} end, 1, StartingList),
  MappedList = lists:droplast(lists:flatten(tuple_to_list(MappedTuple))),
  FilteredList = lists:filter(fun(X) -> factors(X) > Lim end, MappedList),
  FoldedValue=lists:foldr(fun(X, _) -> X end, 0, FilteredList),
  FoldedValue.
