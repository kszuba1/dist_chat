-module(chat_listener).
-export([start/2, init/3, loop/1]).

start(ServerNode, Nick) ->
  io:format("Listener: Starting...~n"),
  Parent = self(),
  Pid = spawn(?MODULE, init, [ServerNode, Parent, Nick]),
  receive
    {subscribe_result, ok} ->
      io:format("Listener: Successfully subscribed as '~s'~n", [Nick]),
      Pid;
    {subscribe_result, error, Reason} ->
      io:format("Listener: Error subscribing! Reason: ~s~n", [Reason]),
      {error, Reason}
  end.

init(ServerNode, Parent, Nick) ->
  {chat_server, ServerNode} ! {subscribe, self(), Nick},
  receive
    {subscribe_result, ok} ->
      Parent ! {subscribe_result, ok},
      loop(ServerNode);
    {subscribe_result, error, Reason} ->
      Parent ! {subscribe_result, error, Reason}
  end.

loop(ServerNode) ->
  receive
    {chat_msg, Author, Message} ->
      io:format("~n[~s]: ~s~n", [Author, Message]),
      loop(ServerNode);

    {private_msg, Author, Message} ->
      io:format("~n(Whisper) [~s]: ~s~n", [Author, Message]),
      loop(ServerNode);

    {say, Message} ->
      {chat_server, ServerNode} ! {broadcast, self(), Message},
      loop(ServerNode);

    {whisper, TargetNick, Message} ->
      {chat_server, ServerNode} ! {private_msg, self(), TargetNick, Message},
      loop(ServerNode);

    who_is_online ->
      {chat_server, ServerNode} ! {get_users, self()},
      loop(ServerNode);

    {users_list, Nicks} ->
      io:format("~n--- Online Users ---~n"),
      print_list(Nicks),
      io:format("--------------------~n"),
      loop(ServerNode);

    Other ->
      io:format("Listener: Received unknown message: ~p~n", [Other]),
      loop(ServerNode)
  end.

print_list([]) -> ok;
print_list([H|T]) ->
  io:format(" * ~s~n", [H]),
  print_list(T).