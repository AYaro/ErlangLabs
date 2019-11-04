-module(property_test).
-import(custom_set_new, [from_list/1, union/2]).
-export([start/0, generate_random_set/0, check_union_is_commutative/1 , check_union_is_commutative/1, check_union_is_idempotent/1, union_is_commutative/0, union_is_idempotent/0, union_is_associative/0]). 

-include_lib("proper/include/proper.hrl").

generate_random_set() ->
    from_list([proper_types:number() || _ <- lists:seq(1, 1000)]).

check_union_is_commutative(Set) ->
    Set2 = generate_random_set(),
    Union1 = union(Set, Set2),
    Union2 = union(Set2, Set),

    if 
        (Union1 =:= Union2) -> true;
        true -> false
    end.

union_is_commutative() ->
    ?FORALL(
      Set,
      generate_random_set(),
      check_union_is_commutative(Set)
    ).

check_union_is_associative(Set) -> 
    Set2 = generate_random_set(),
    Set3 = generate_random_set(),
    Union2_3 = union(Set2, Set3),
    Union1_23 = union(Set, Union2_3),
    Union1_2 = union(Set, Set2),
    Union12_3 = union(Union1_2, Set3),
    if 
        (Union1_23 =:= Union12_3) -> true;
        true -> false
    end.

union_is_associative() ->
    ?FORALL(
      Set,
      generate_random_set(),
      check_union_is_associative(Set)
    ).


check_union_is_idempotent(Set) ->
    Set2 = Set,
    Union1_2 = union(Set, Set2),
    if (Set =:= Union1_2) -> true;
        true -> false
    end.

union_is_idempotent() ->
    ?FORALL(
      Set,
      generate_random_set(),
      check_union_is_idempotent(Set)
    ).

start() -> 
    proper:quickcheck(property_test:union_is_commutative()),
    proper:quickcheck(property_test:union_is_associative()),
    proper:quickcheck(property_test:union_is_idempotent()),
    io:fwrite("~n").

