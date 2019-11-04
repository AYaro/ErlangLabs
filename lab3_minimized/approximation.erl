-module(approximation).
-export([approximate/5]).
-import(point, [x/1, y/1]).

approximate(X1, X2, Y1, Y2, X) -> 
    Y2 + ((Y1 - Y2) / (X1 - X2)) * (X - X2).