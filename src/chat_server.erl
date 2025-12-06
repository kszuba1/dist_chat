-module(chat_server).
-author("kszuba").
-export([start/0, loop/1]).

%% @doc Starts the chat server process and registers it globally.
start() ->
  io:format("Chat server starting...~n"),
  Pid = spawn(?MODULE, loop, [[]]),
  register(chat_server, Pid),
  io:format("Chat server started with Pid ~p~n", [Pid]),
  Pid.

%% @doc The main server loop. Clients is a list of {Nick, Pid}.
loop(Clients) ->
  receive
    {subscribe, ListenerPid, Nick} ->
      case lists:keymember(Nick, 1, Clients) of
        true ->
          io:format("Server: Nick '~s' is already taken.~n", [Nick]),
          ListenerPid ! {subscribe_result, error, "Nick taken"},
          loop(Clients);
        false ->
          io:format("Server: New user '~s' joined (~p)~n", [Nick, ListenerPid]),
          ListenerPid ! {subscribe_result, ok},
          loop([{Nick, ListenerPid} | Clients])
      end;

    {broadcast, SenderPid, Message} ->
      case lists:keyfind(SenderPid, 2, Clients) of
        {SenderNick, _} ->
          io:format("Server: Broadcasting message from '~s'~n", [SenderNick]),
          [Pid ! {chat_msg, SenderNick, Message} || {_, Pid} <- Clients],
          loop(Clients);
        false ->
          io:format("Server: Unknown sender ~p tried to broadcast~n", [SenderPid]),
          loop(Clients)
      end;

  %% NEW: Handle "Who is online" request
    {get_users, RequesterPid} ->
      io:format("Server: Sending user list to ~p~n", [RequesterPid]),
      %% Extract only the Nicknames from the state tuples
      %% Clients = [{Nick, Pid}, ...] -> Nicks = [Nick, ...]
      Nicks = [N || {N, _} <- Clients],
      RequesterPid ! {users_list, Nicks},
      loop(Clients);

    Other ->
      io:format("Server: Received unknown message: ~p~n", [Other]),
      loop(Clients)
  end.