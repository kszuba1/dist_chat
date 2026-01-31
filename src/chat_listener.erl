-module(chat_listener).
-export([start/2, init/3, loop/1]).

start(ServerNode, Nick) ->
  chat_ui:format_info("Connecting to chat server..."),
  Parent = self(),
  Pid = spawn(?MODULE, init, [ServerNode, Parent, Nick]),
  receive
    {subscribe_result, ok} ->
      chat_ui:format_success("Successfully joined as '" ++ Nick ++ "'"),
      print_welcome(Nick),
      Pid;
    {subscribe_result, error, Reason} ->
      chat_ui:format_error("Failed to subscribe: " ++ Reason),
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
      chat_ui:format_chat_msg(Author, Message),
      loop(ServerNode);

    {private_msg, Author, Message} ->
      chat_ui:format_whisper(Author, Message),
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
      print_users_box(Nicks),
      loop(ServerNode);

    %% Room commands
    {create_room, RoomName} ->
      {chat_server, ServerNode} ! {create_room, self(), RoomName},
      loop(ServerNode);

    {join_room, RoomName} ->
      {chat_server, ServerNode} ! {join_room, self(), RoomName},
      loop(ServerNode);

    {leave_room, RoomName} ->
      {chat_server, ServerNode} ! {leave_room, self(), RoomName},
      loop(ServerNode);

    {room_say, RoomName, Message} ->
      {chat_server, ServerNode} ! {room_msg, self(), RoomName, Message},
      loop(ServerNode);

    list_rooms ->
      {chat_server, ServerNode} ! {get_rooms, self()},
      loop(ServerNode);

    {room_users, RoomName} ->
      {chat_server, ServerNode} ! {get_room_users, self(), RoomName},
      loop(ServerNode);

    %% Room responses from server
    {room_msg, RoomName, Author, Message} ->
      chat_ui:format_room_msg(RoomName, Author, Message),
      loop(ServerNode);

    {rooms_list, RoomNames} ->
      print_rooms_box(RoomNames),
      loop(ServerNode);

    {room_users_list, RoomName, Nicks} ->
      print_room_users_box(RoomName, Nicks),
      loop(ServerNode);

    {room_created, RoomName} ->
      chat_ui:format_success("Room '" ++ RoomName ++ "' created and joined"),
      loop(ServerNode);

    {room_joined, RoomName} ->
      chat_ui:format_success("Joined room '" ++ RoomName ++ "'"),
      loop(ServerNode);

    {room_left, RoomName} ->
      chat_ui:format_info("Left room '" ++ RoomName ++ "'"),
      loop(ServerNode);

    {room_error, Reason} ->
      chat_ui:format_error(Reason),
      loop(ServerNode);

    Other ->
      chat_ui:format_error(io_lib:format("Unknown message: ~p", [Other])),
      loop(ServerNode)
  end.

%% Print welcome banner
print_welcome(Nick) ->
  io:format("~n"),
  io:format("  ~s~n", [chat_ui:cyan("+=============================================+")]),
  io:format("  ~s~s~s~n", [chat_ui:cyan("|"), chat_ui:bold("       Welcome to Distributed Chat!        "), chat_ui:cyan("|")]),
  io:format("  ~s~n", [chat_ui:cyan("+=============================================+")]),
  io:format("  ~s~n", [chat_ui:cyan("|") ++ "  General Commands:                        " ++ chat_ui:cyan("|")]),
  io:format("  ~s~n", [chat_ui:cyan("|") ++ "    Pid ! {say, \"message\"}                 " ++ chat_ui:cyan("|")]),
  io:format("  ~s~n", [chat_ui:cyan("|") ++ "    Pid ! {whisper, \"nick\", \"msg\"}         " ++ chat_ui:cyan("|")]),
  io:format("  ~s~n", [chat_ui:cyan("|") ++ "    Pid ! who_is_online                    " ++ chat_ui:cyan("|")]),
  io:format("  ~s~n", [chat_ui:cyan("+---------------------------------------------+")]),
  io:format("  ~s~n", [chat_ui:cyan("|") ++ "  Room Commands:                           " ++ chat_ui:cyan("|")]),
  io:format("  ~s~n", [chat_ui:cyan("|") ++ "    Pid ! {create_room, \"name\"}            " ++ chat_ui:cyan("|")]),
  io:format("  ~s~n", [chat_ui:cyan("|") ++ "    Pid ! {join_room, \"name\"}              " ++ chat_ui:cyan("|")]),
  io:format("  ~s~n", [chat_ui:cyan("|") ++ "    Pid ! {leave_room, \"name\"}             " ++ chat_ui:cyan("|")]),
  io:format("  ~s~n", [chat_ui:cyan("|") ++ "    Pid ! {room_say, \"name\", \"msg\"}        " ++ chat_ui:cyan("|")]),
  io:format("  ~s~n", [chat_ui:cyan("|") ++ "    Pid ! list_rooms                       " ++ chat_ui:cyan("|")]),
  io:format("  ~s~n", [chat_ui:cyan("|") ++ "    Pid ! {room_users, \"name\"}             " ++ chat_ui:cyan("|")]),
  io:format("  ~s~n", [chat_ui:cyan("+=============================================+")]),
  io:format("  ~s~n", [chat_ui:dim("  Your nickname: ") ++ chat_ui:green(Nick)]),
  io:format("  ~s~n~n", [chat_ui:dim("  Auto-joined room: ") ++ chat_ui:yellow("general")]).

%% Print users in a nice box
print_users_box(Nicks) ->
  io:format("~n"),
  io:format("  ~s~n", [chat_ui:cyan("+-----------------------------+")]),
  io:format("  ~s~s~s~n", [chat_ui:cyan("|"), chat_ui:bold("       Online Users          "), chat_ui:cyan("|")]),
  io:format("  ~s~n", [chat_ui:cyan("+-----------------------------+")]),
  print_users_list(Nicks),
  io:format("  ~s~n", [chat_ui:cyan("+-----------------------------+")]),
  io:format("  ~s~n~n", [chat_ui:dim("  Total: " ++ integer_to_list(length(Nicks)) ++ " user(s)")]).

print_users_list([]) -> ok;
print_users_list([Nick|T]) ->
  StatusDot = chat_ui:status_dot(online),
  Padded = string:pad(Nick, 24, trailing),
  io:format("  ~s ~s ~s~s~n", [chat_ui:cyan("|"), StatusDot, Padded, chat_ui:cyan("|")]),
  print_users_list(T).

%% Print rooms in a nice box
print_rooms_box(RoomNames) ->
  io:format("~n"),
  io:format("  ~s~n", [chat_ui:yellow("+-----------------------------+")]),
  io:format("  ~s~s~s~n", [chat_ui:yellow("|"), chat_ui:bold("       Available Rooms       "), chat_ui:yellow("|")]),
  io:format("  ~s~n", [chat_ui:yellow("+-----------------------------+")]),
  print_rooms_list(RoomNames),
  io:format("  ~s~n", [chat_ui:yellow("+-----------------------------+")]),
  io:format("  ~s~n~n", [chat_ui:dim("  Total: " ++ integer_to_list(length(RoomNames)) ++ " room(s)")]).

print_rooms_list([]) ->
  io:format("  ~s~s~s~n", [chat_ui:yellow("|"), chat_ui:dim("  (no rooms available)       "), chat_ui:yellow("|")]);
print_rooms_list([Room|T]) ->
  Padded = string:pad(Room, 26, trailing),
  io:format("  ~s ~s~s~s~n", [chat_ui:yellow("|"), chat_ui:cyan("#"), Padded, chat_ui:yellow("|")]),
  print_rooms_list(T).

%% Print room users in a nice box
print_room_users_box(RoomName, Nicks) ->
  io:format("~n"),
  Header = "  Room: #" ++ RoomName ++ "  ",
  HeaderPadded = string:pad(Header, 29, trailing),
  io:format("  ~s~n", [chat_ui:magenta("+-----------------------------+")]),
  io:format("  ~s~s~s~n", [chat_ui:magenta("|"), chat_ui:bold(HeaderPadded), chat_ui:magenta("|")]),
  io:format("  ~s~n", [chat_ui:magenta("+-----------------------------+")]),
  print_room_users_list(Nicks),
  io:format("  ~s~n", [chat_ui:magenta("+-----------------------------+")]),
  io:format("  ~s~n~n", [chat_ui:dim("  Members: " ++ integer_to_list(length(Nicks)))]).

print_room_users_list([]) ->
  io:format("  ~s~s~s~n", [chat_ui:magenta("|"), chat_ui:dim("  (room is empty)            "), chat_ui:magenta("|")]);
print_room_users_list([Nick|T]) ->
  StatusDot = chat_ui:status_dot(online),
  Padded = string:pad(Nick, 24, trailing),
  io:format("  ~s ~s ~s~s~n", [chat_ui:magenta("|"), StatusDot, Padded, chat_ui:magenta("|")]),
  print_room_users_list(T).
