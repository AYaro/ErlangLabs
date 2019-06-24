-module (tail_recursion).
-export ([triangular/4, factors/3, start/1]).

triangular(N, Facs, Value, Lim) when Facs < Lim ->
  triangular(N+1, factors(Value+N+1, 1, 0), Value+N+1, Lim);
triangular(N, Facs, Value, Lim) -> Value.

factors(N, F, Count) when (N rem F) == 0 ->
  Lim =  math:ceil(math:sqrt(N)),
  if
    F >= Lim -> Count;
    true -> 0+factors(N, F+1, Count + 2)
  end;
factors(N, F, Count) -> 0+factors(N, F+1, Count).

start(Lim) ->
  triangular(2, 0, 3, Lim).
