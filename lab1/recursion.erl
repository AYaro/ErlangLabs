-module (recursion).
-export ([triangular/1, factors/1, start/1, get_result/2]).

triangular(1) -> 1;
triangular(N) when N > 1 -> N + triangular(N-1).

factors(N) -> factors(N, 1, 0).
factors(N, F, Count) when ((N rem F) == 0) ->
  Lim =  math:ceil(math:sqrt(N)),
  if
    F >= Lim -> Count;
    true -> factors(N, F+1, Count + 2)
  end;
factors(N, F, Count) ->   factors(N, F+1, Count).

get_result(Count, Lim) ->
  Result = triangular(Count),
  Factors = factors(Result),
  if
    Factors >= Lim -> Result;
    true -> get_result(Count+1, Lim)
  end.

start(Lim) ->
  get_result(1, Lim).
