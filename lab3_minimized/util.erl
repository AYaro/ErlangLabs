-module(util). 
-export([parse_args/1, input/1, str_to_point/1]).

% approximation args
parse_args(Args) when length(Args) =:= 3 ->
   [Step , Start_X, Start_Y] = Args,
   {str_to_float(atom_to_list(Step)),
      str_to_float(atom_to_list(Start_X)),  
      str_to_float(atom_to_list(Start_Y))}.

input(Fun) ->
   io:format("enter point~n"), 
   Line = io:get_line(""),
   if 
      (Line =:= "stop\n") -> stop;
      true ->  Fun(Line)          
   end.

str_to_point(Str) ->
   Point_Tokens = string:tokens(Str, ";"),
   [ X_String | Y_String ] = Point_Tokens,
   X = str_to_float(X_String),
   Y = str_to_float(string:trim(Y_String)),
   {X, Y}.

str_to_float(Str) ->
   try float(list_to_integer(Str))
   catch error:_ -> list_to_float(Str)
   end. 
