-module(chat_server).
-author("kszuba").
-export([start/0, print_users/0, loop/1]).

start() ->
  io:format("Chat server starting...~n"),
  Pid = spawn(?MODULE, loop, [[]]),
  register(chat_server, Pid),
  io:format("Chat server started with Pid ~p~n", [Pid]),
  Pid.

print_users() ->
  chat_server ! show_users_console,
  ok.

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

  %% NEW: Handle private message (Whisper)
    {private_msg, SenderPid, TargetNick, Message} ->
      case lists:keyfind(SenderPid, 2, Clients) of
        {SenderNick, _} ->
          case lists:keyfind(TargetNick, 1, Clients) of
            {_, TargetPid} ->
              io:format("Server: Whisper from '~s' to '~s'~n", [SenderNick, TargetNick]),
              TargetPid ! {private_msg, SenderNick, Message},
              loop(Clients);
            false ->
              io:format("Server: User '~s' tried to whisper to non-existent '~s'~n", [SenderNick, TargetNick]),
              SenderPid ! {chat_msg, "SERVER", "User '" ++ TargetNick ++ "' not found."},
              loop(Clients)
          end;
        false ->
          io:format("Server: Unknown sender ~p tried to whisper~n", [SenderPid]),
          loop(Clients)
      end;

    {get_users, RequesterPid} ->
      io:format("Server: Sending user list to ~p~n", [RequesterPid]),
      Nicks = [N || {N, _} <- Clients],
      RequesterPid ! {users_list, Nicks},
      loop(Clients);

    show_users_console ->
      io:format("~n--- Admin: Connected Users ---~n"),
      print_clients_internal(Clients),
      io:format("------------------------------~n"),
      loop(Clients);

    Other ->
      io:format("Server: Received unknown message: ~p~n", [Other]),
      loop(Clients)
  end.

%% Helper to print the list of tuples
print_clients_internal([]) -> ok;
print_clients_internal([{Nick, Pid} | T]) ->
  io:format(" User: ~-10s | Pid: ~p~n", [Nick, Pid]),
  print_clients_internal(T).