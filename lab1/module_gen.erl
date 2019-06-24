-module (module_gen).
-export ([triangular/1, start/1]).

triangular(N) -> triangular(N,1, 1, []).
triangular (1, Count, Sum, List) -> lists:reverse([Sum | List]);
triangular (N, Count, Sum, List) when N > 1 -> triangular (N-1 , Count+1, Sum + Count + 1,   [ Sum | List]).

factors(N) -> factors(N, 1, 0).
factors(N, F, Count) when ((N rem F) == 0) ->
  Lim =  math:ceil(math:sqrt(N)),
  if
    F >= Lim -> Count;
    true -> factors(N, F+1, Count + 2)
  end;
factors(N, F, Count) -> factors(N, F+1, Count).

start(Lim) ->
  StartingList = triangular(13000),
  FilteredList = lists:filter(fun(X) -> factors(X) > Lim end, StartingList),
  FoldedValue=lists:foldr(fun(X, _) -> X end, 0, FilteredList),
  FoldedValue.
