-module(chat_listener).
-export([start/2, init/3, loop/1]).

%% @doc Starts the listener.
start(ServerNode, Nick) ->
  io:format("Listener: Starting...~n"),
  Parent = self(), %% Save the Shell's PID

  %% Spawn the 'init' function instead of 'loop' directly
  Pid = spawn(?MODULE, init, [ServerNode, Parent, Nick]),

  %% Wait for the spawned process to tell us it's ready
  receive
    {subscribe_result, ok} ->
      io:format("Listener: Successfully subscribed as '~s'~n", [Nick]),
      Pid;
    {subscribe_result, error, Reason} ->
      io:format("Listener: Error subscribing! Reason: ~s~n", [Reason]),
      %% The spawned process will exit on its own
      {error, Reason}
  end.

%% @doc Internal initialization.
init(ServerNode, Parent, Nick) ->
  %% Send subscription request to server
  {chat_server, ServerNode} ! {subscribe, self(), Nick},

  %% Wait for Server reply
  receive
    {subscribe_result, ok} ->
      %% Forward success to the Shell (Parent) so 'start' can return
      Parent ! {subscribe_result, ok},
      %% Now enter the main loop
      loop(ServerNode);

    {subscribe_result, error, Reason} ->
      %% Forward error to Shell (Parent)
      Parent ! {subscribe_result, error, Reason}
  %% Process dies here naturally
  end.

%% @doc Main listener loop (No changes needed here)
loop(ServerNode) ->
  receive
    {chat_msg, Author, Message} ->
      io:format("~n[~s]: ~s~n", [Author, Message]),
      loop(ServerNode);

    {say, Message} ->
      {chat_server, ServerNode} ! {broadcast, self(), Message},
      loop(ServerNode);

  %% NEW: User command to check online users
    who_is_online ->
      %% Send request to server
      {chat_server, ServerNode} ! {get_users, self()},
      loop(ServerNode);

  %% NEW: Handle the response from server
    {users_list, Nicks} ->
      io:format("~n--- Online Users ---~n"),
      print_list(Nicks),
      io:format("--------------------~n"),
      loop(ServerNode);

    Other ->
      io:format("Listener: Received unknown message: ~p~n", [Other]),
      loop(ServerNode)
  end.

%% Helper function to print list nicely
print_list([]) -> ok;
print_list([H|T]) ->
  io:format(" * ~s~n", [H]),
  print_list(T).