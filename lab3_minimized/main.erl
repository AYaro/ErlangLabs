-module(main). 
-export([main/1, reader/0, printer/1, approximator/3]).
-import(util, [parse_args/1, input/1, str_to_point/1]).
-import(approximation,[approximate/5]).


reader() ->
    Point = input(fun(Line) -> str_to_point(Line) end),
    if 
        (Point =:= stop) -> ok;
        true ->
            {X, Y} = Point,
            approximator ! {calculate, X, Y},
            reader()
    end.

printer(Prefix) ->
    receive 
        {result, X, Y} ->
            io:format("~s ~f ~f~n",[Prefix, X,Y]),
            printer(Prefix);
        {stop} -> 
            io:format("Stopping printer~n");
        Other ->
            io:format("Unknown: ~p~n", [Other])
    end.

approximator(Step, X1, Y1) ->
    receive 
        {calculate, X2, Y2} ->
            approximation_loop(X1, X2, Y1, Y2, Step, X1),
            approximator(Step, X2, Y2);
        {stop} -> 
            io:format("Stopping approximator~n");
        Other ->
            io:format("Unknown: ~p~n", [Other])
    end.

approximation_loop(X1, X2, Y1, Y2, Step, Current_X) when Current_X < X2->
    Result = approximate(X1, X2, Y1, Y2, Current_X),
    printer ! {result, Current_X, Result},
    approximation_loop(X1, X2, Y1, Y2, Step, Current_X + Step);

approximation_loop(_, _, _, _, _, _) ->
    ok.


main(Args) ->
    io:format("Taken args: ~p\n", [Args]),
    { Step, Start_X, Start_Y } = parse_args(Args),
    register(printer, spawn(main, printer, ["Result: "])),
    register(approximator, spawn(main, approximator, [Step, Start_X, Start_Y])),
    reader().