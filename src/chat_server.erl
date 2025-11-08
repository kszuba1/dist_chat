-module(chat_server).
-author("kszuba").
-export([start/0, loop/1]).

%% start the chat server process and registers it.
start() ->
  io:format("Chat server starting...~n"),
  % create loop function with an empty list of clients
  Pid = spawn(?MODULE, loop, [[]]),
  % register the process with the name 'chat_server'.
  register(chat_server, Pid),
  io:format("Chat server started with Pid ~p~n", [Pid]),
  Pid.

% main server loop. It waits for messages.
% state - list of Pids of all connected listeners.
loop(Listeners) ->
  receive
  % new listener wants to subscribe
    {subscribe, ListenerPid} ->
      io:format("Server: New listener subscribed: ~p~n", [ListenerPid]),
      % Add the new listener to the list and loop again
      loop([ListenerPid | Listeners]);

  %  broadcast message is received
    {broadcast, Message} ->
      io:format("Server: Broadcasting '~p' to ~p listeners~n", [Message, length(Listeners)]),
      % send the chat message to every listener in the list
      [Listener ! {chat_msg, Message} || Listener <- Listeners],
      % loop again with the same list of listeners
      loop(Listeners);

    Other ->
      io:format("Server: Received unknown message: ~p~n", [Other]),
      loop(Listeners)
  end.
