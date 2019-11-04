-module(person).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {chat_node, profile}).

-export([login/1, logout/0, say/1, users/0, who/2, set_profile/2]). 

-define(CLIENT, ?MODULE).

start_link(ChatNode) ->
  gen_server:start_link({local, ?CLIENT}, ?MODULE, ChatNode, []).

init(ChatNode)->
  io:format("Chat node is: ~p~n", [ChatNode]),
  {ok, #state{chat_node=ChatNode, profile=[]}}.

handle_call(get_chat_node, _From, State) ->
  {reply, State#state.chat_node, State};

handle_call(get_profile, _From, State) ->
  {reply, State#state.profile, State};
  
handle_call({set_profile, Key, Value}, _From, State) ->
  case lists:keymember(Key, 1, State#state.profile) of
    true -> NewProfile = lists:keyreplace(Key, 1, State#state.profile,
      {Key, Value});
    false -> NewProfile = [{Key, Value} | State#state.profile]
  end,
  {reply, NewProfile,
    #state{chat_node = State#state.chat_node, profile=NewProfile}};

handle_call({login, UserName}, _From, State) ->
  Reply = gen_server:call({chatroom, State#state.chat_node},
    {login, UserName, node()}),
  {reply, Reply, State};
  
handle_call({say, Text}, _From, State) ->
  Reply = gen_server:call({chatroom, State#state.chat_node},
    {say, Text}),
  {reply, Reply, State};

handle_call(logout, _From, State) ->
  Reply = gen_server:call({chatroom, State#state.chat_node}, logout),
  {reply, Reply, State};

handle_call(_, _From, State) -> {ok, [], State}.

handle_cast({message, {FromUser, FromServer}, Text}, State) ->
  io:format("~s (~p) says: ~p~n", [FromUser, FromServer, Text]),
  {noreply, State};
  
handle_cast(_Request, State) ->
  io:format("Unknown request ~p~n", _Request),
  {noReply, State}.

handle_info(Info, State) ->
  io:format("Received unexpected message: ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


% internal functions

get_chat_node() ->
  gen_server:call(person, get_chat_node).

login(UserName) ->
  if
    is_atom(UserName) ->
      gen_server:call(?CLIENT,
        {login, atom_to_list(UserName)});
    is_list(UserName) ->
      gen_server:call(?CLIENT,
        {login, UserName});
    true ->
      {error, "User name must be an atom or a list"}
  end.

logout() ->
  gen_server:call(?CLIENT, logout),
  ok.

say(Text) ->
  gen_server:call(?CLIENT, {say, Text}),
  ok.

users() ->
  gen_server:call({chatroom, get_chat_node()}, users).

who(Person, ServerRef) ->
  gen_server:call({chatroom, get_chat_node()},
    {who, Person, ServerRef}).

set_profile(Key, Value) ->
  NewProfile = gen_server:call(?CLIENT, {set_profile, Key, Value}),
  {ok, NewProfile}.