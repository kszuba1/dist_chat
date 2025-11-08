-module(chat_listener).
-export([start/1, loop/0]).

start(ServerNode) ->
  io:format("Listener: Starting on node ~p...~n", [node()]),
  % create listener's loop
  Pid = spawn(?MODULE, loop, []),

  % send a 'subscribe' message to the registered 'chat_server' process, which lives on the ServerNode.
  {chat_server, ServerNode} ! {subscribe, Pid},
  io:format("Listener: Subscribed to ~p~n", [{chat_server, ServerNode}]),
  Pid.

% listener's loop waiting for messages from the server.
loop() ->
  receive
  % message broadcast from the server
    {chat_msg, Message} ->
      io:format("~n>>> Received message: ~s~n", [Message]),
      loop();

    Other ->
      io:format("Listener: Received unknown message: ~p~n", [Other]),
      loop()
  end.