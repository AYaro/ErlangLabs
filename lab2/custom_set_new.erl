-module(custom_set_new).

-export([add/2, delete/2, from_list/1,
	 union/2, foldr/3, foldl/3, map/2, new/0, binary_search/2]).

new () -> [].

add (Elem, [])          -> [Elem];
add (Elem, Set) ->
    Search = binary_search(Set, Elem), 
    if 
        (element(1,Search) == false) -> Set;
        (element(2,Search) == 0)  -> [Elem | Set];
        (element(2,Search) == length(Set)) -> Set ++ [Elem];
        true -> sublist(Set, element(2,Search)) ++ [Elem] ++ nthtail(element(2,Search), Set)
    end.

delete(Elem, Set) -> lists:delete(Elem, Set).

from_list(List) ->
    foldr(fun add/2, [], List).

union(Set1, Set2) ->
    foldr(fun add/2, Set1, Set2).

foldl(_ , Acc, []) -> Acc;
foldl(Fun, Acc, [Head | Tail]) -> 
    foldl(Fun, Fun(Head, Acc), Tail).

foldr(_ , Acc, []) -> Acc;
foldr(Fun, Acc, [Head|Tail]) ->
    Fun(Head, foldr(Fun, Acc, Tail)).

map(F, [H|T]) ->
    L = [F(H)|map(F, T)],
    from_list(L);
map(F, []) when is_function(F, 1) -> [].

binary_search(List, N) ->
  binary_search(N, 1, length(List), List).

binary_search(_N, Left, Right, _OriList ) when Left > Right ->
    {true,(Left + Right) div 2};
binary_search( N, Left, Right, OriList ) when Left =< Right ->

  Middle = (Left + Right) div 2, 

  Item = nth(Middle, OriList),

  case Item of
    N -> {false, Middle}; %% yay, found it!
    _ -> case Item > N of
           true  -> binary_search( N, Left, Middle-1,  OriList); %% left
           false -> binary_search( N, Middle+1, Right , OriList)           %% right
         end
  end.

nth(1, [H|_]) -> H;
nth(N, [_|T]) when N > 1 ->
    nth(N - 1, T).

nthtail(1, [_|T]) -> T;
nthtail(N, [_|T]) when N > 1 ->
    nthtail(N - 1, T);
nthtail(0, L) when is_list(L) -> L.

sublist([H|T], L) when L > 0 ->
    [H|sublist(T, L-1)];
sublist(_, 0) ->
    [];
sublist(List, L) when is_list(List), L > 0 ->
    [].
