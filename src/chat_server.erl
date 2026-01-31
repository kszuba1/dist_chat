-module(chat_server).
-author("kszuba").
-export([start/0, print_users/0, loop/2]).

start() ->
  print_server_banner(),
  chat_ui:format_info("Initializing chat server..."),
  %% Initialize with empty Clients and Rooms with default "general" room
  Rooms = #{"general" => []},
  Pid = spawn(?MODULE, loop, [[], Rooms]),
  register(chat_server, Pid),
  chat_ui:format_success(io_lib:format("Server started (PID: ~p)", [Pid])),
  chat_ui:format_info("Default room 'general' created"),
  io:format("~n"),
  Pid.

print_users() ->
  chat_server ! show_users_console,
  ok.

loop(Clients, Rooms) ->
  receive
    {subscribe, ListenerPid, Nick} ->
      case lists:keymember(Nick, 1, Clients) of
        true ->
          chat_ui:format_error(io_lib:format("Nickname '~s' is already taken", [Nick])),
          ListenerPid ! {subscribe_result, error, "Nick taken"},
          loop(Clients, Rooms);
        false ->
          chat_ui:format_success(io_lib:format("New user joined: ~s", [Nick])),
          print_user_count(length(Clients) + 1),
          ListenerPid ! {subscribe_result, ok},
          %% Notify other users about new join
          [Pid ! {chat_msg, "SERVER", Nick ++ " has joined the chat!"} || {_, Pid} <- Clients],
          %% Auto-join user to "general" room
          NewRooms = add_user_to_room(Nick, "general", Rooms),
          loop([{Nick, ListenerPid} | Clients], NewRooms)
      end;

    {broadcast, SenderPid, Message} ->
      %% Backward compatible: broadcast to "general" room
      case lists:keyfind(SenderPid, 2, Clients) of
        {SenderNick, _} ->
          chat_ui:format_info(io_lib:format("Broadcasting from '~s' to 'general'", [SenderNick])),
          broadcast_to_room("general", SenderNick, Message, Clients, Rooms),
          loop(Clients, Rooms);
        false ->
          chat_ui:format_error(io_lib:format("Unknown sender: ~p", [SenderPid])),
          loop(Clients, Rooms)
      end;

    {private_msg, SenderPid, TargetNick, Message} ->
      case lists:keyfind(SenderPid, 2, Clients) of
        {SenderNick, _} ->
          case lists:keyfind(TargetNick, 1, Clients) of
            {_, TargetPid} ->
              chat_ui:format_info(io_lib:format("Whisper: ~s -> ~s", [SenderNick, TargetNick])),
              TargetPid ! {private_msg, SenderNick, Message},
              loop(Clients, Rooms);
            false ->
              chat_ui:format_error(io_lib:format("User '~s' not found for whisper", [TargetNick])),
              SenderPid ! {chat_msg, "SERVER", "User '" ++ TargetNick ++ "' not found."},
              loop(Clients, Rooms)
          end;
        false ->
          chat_ui:format_error(io_lib:format("Unknown sender: ~p", [SenderPid])),
          loop(Clients, Rooms)
      end;

    {get_users, RequesterPid} ->
      chat_ui:format_info("User list requested"),
      Nicks = [N || {N, _} <- Clients],
      RequesterPid ! {users_list, Nicks},
      loop(Clients, Rooms);

    %% Room management
    {create_room, SenderPid, RoomName} ->
      case lists:keyfind(SenderPid, 2, Clients) of
        {SenderNick, _} ->
          case maps:is_key(RoomName, Rooms) of
            true ->
              chat_ui:format_error(io_lib:format("Room '~s' already exists", [RoomName])),
              SenderPid ! {room_error, "Room '" ++ RoomName ++ "' already exists"},
              loop(Clients, Rooms);
            false ->
              chat_ui:format_success(io_lib:format("Room '~s' created by ~s", [RoomName, SenderNick])),
              %% Create room with creator as first member
              NewRooms = Rooms#{RoomName => [SenderNick]},
              SenderPid ! {room_created, RoomName},
              loop(Clients, NewRooms)
          end;
        false ->
          chat_ui:format_error(io_lib:format("Unknown sender: ~p", [SenderPid])),
          loop(Clients, Rooms)
      end;

    {join_room, SenderPid, RoomName} ->
      case lists:keyfind(SenderPid, 2, Clients) of
        {SenderNick, _} ->
          case maps:is_key(RoomName, Rooms) of
            false ->
              chat_ui:format_error(io_lib:format("Room '~s' does not exist", [RoomName])),
              SenderPid ! {room_error, "Room '" ++ RoomName ++ "' does not exist"},
              loop(Clients, Rooms);
            true ->
              Members = maps:get(RoomName, Rooms),
              case lists:member(SenderNick, Members) of
                true ->
                  SenderPid ! {room_error, "You are already in room '" ++ RoomName ++ "'"},
                  loop(Clients, Rooms);
                false ->
                  chat_ui:format_info(io_lib:format("~s joined room '~s'", [SenderNick, RoomName])),
                  NewRooms = add_user_to_room(SenderNick, RoomName, Rooms),
                  SenderPid ! {room_joined, RoomName},
                  %% Notify room members
                  broadcast_to_room(RoomName, "SERVER", SenderNick ++ " joined the room", Clients, NewRooms),
                  loop(Clients, NewRooms)
              end
          end;
        false ->
          chat_ui:format_error(io_lib:format("Unknown sender: ~p", [SenderPid])),
          loop(Clients, Rooms)
      end;

    {leave_room, SenderPid, RoomName} ->
      case lists:keyfind(SenderPid, 2, Clients) of
        {SenderNick, _} ->
          case maps:is_key(RoomName, Rooms) of
            false ->
              SenderPid ! {room_error, "Room '" ++ RoomName ++ "' does not exist"},
              loop(Clients, Rooms);
            true ->
              Members = maps:get(RoomName, Rooms),
              case lists:member(SenderNick, Members) of
                false ->
                  SenderPid ! {room_error, "You are not in room '" ++ RoomName ++ "'"},
                  loop(Clients, Rooms);
                true ->
                  chat_ui:format_info(io_lib:format("~s left room '~s'", [SenderNick, RoomName])),
                  NewRooms = remove_user_from_room(SenderNick, RoomName, Rooms),
                  SenderPid ! {room_left, RoomName},
                  %% Notify remaining room members
                  broadcast_to_room(RoomName, "SERVER", SenderNick ++ " left the room", Clients, NewRooms),
                  loop(Clients, NewRooms)
              end
          end;
        false ->
          chat_ui:format_error(io_lib:format("Unknown sender: ~p", [SenderPid])),
          loop(Clients, Rooms)
      end;

    {room_msg, SenderPid, RoomName, Message} ->
      case lists:keyfind(SenderPid, 2, Clients) of
        {SenderNick, _} ->
          case maps:is_key(RoomName, Rooms) of
            false ->
              SenderPid ! {room_error, "Room '" ++ RoomName ++ "' does not exist"},
              loop(Clients, Rooms);
            true ->
              Members = maps:get(RoomName, Rooms),
              case lists:member(SenderNick, Members) of
                false ->
                  SenderPid ! {room_error, "You are not in room '" ++ RoomName ++ "'"},
                  loop(Clients, Rooms);
                true ->
                  chat_ui:format_info(io_lib:format("Room msg from '~s' to '~s'", [SenderNick, RoomName])),
                  broadcast_to_room(RoomName, SenderNick, Message, Clients, Rooms),
                  loop(Clients, Rooms)
              end
          end;
        false ->
          chat_ui:format_error(io_lib:format("Unknown sender: ~p", [SenderPid])),
          loop(Clients, Rooms)
      end;

    {get_rooms, RequesterPid} ->
      chat_ui:format_info("Room list requested"),
      RoomNames = maps:keys(Rooms),
      RequesterPid ! {rooms_list, RoomNames},
      loop(Clients, Rooms);

    {get_room_users, RequesterPid, RoomName} ->
      case maps:is_key(RoomName, Rooms) of
        false ->
          RequesterPid ! {room_error, "Room '" ++ RoomName ++ "' does not exist"},
          loop(Clients, Rooms);
        true ->
          Members = maps:get(RoomName, Rooms),
          RequesterPid ! {room_users_list, RoomName, Members},
          loop(Clients, Rooms)
      end;

    show_users_console ->
      print_admin_panel(Clients),
      loop(Clients, Rooms);

    Other ->
      chat_ui:format_error(io_lib:format("Unknown message: ~p", [Other])),
      loop(Clients, Rooms)
  end.

%% Room helper functions
add_user_to_room(Nick, RoomName, Rooms) ->
  Members = maps:get(RoomName, Rooms, []),
  case lists:member(Nick, Members) of
    true -> Rooms;
    false -> Rooms#{RoomName => [Nick | Members]}
  end.

remove_user_from_room(Nick, RoomName, Rooms) ->
  Members = maps:get(RoomName, Rooms, []),
  NewMembers = lists:delete(Nick, Members),
  Rooms#{RoomName => NewMembers}.

broadcast_to_room(RoomName, Author, Message, Clients, Rooms) ->
  Members = maps:get(RoomName, Rooms, []),
  [Pid ! {room_msg, RoomName, Author, Message}
   || {Nick, Pid} <- Clients, lists:member(Nick, Members)].

%% Server startup banner
print_server_banner() ->
  io:format("~n"),
  io:format("  ~s~n", [chat_ui:magenta("+=============================================+")]),
  io:format("  ~s~s~s~n", [chat_ui:magenta("|"), chat_ui:bold("        DISTRIBUTED CHAT SERVER           "), chat_ui:magenta("|")]),
  io:format("  ~s~s~s~n", [chat_ui:magenta("|"), chat_ui:dim("            Erlang/OTP Edition            "), chat_ui:magenta("|")]),
  io:format("  ~s~n", [chat_ui:magenta("+=============================================+")]),
  io:format("~n").

%% Print current user count
print_user_count(Count) ->
  io:format("  ~s~n", [chat_ui:dim("  [" ++ integer_to_list(Count) ++ " user(s) online]")]).

%% Admin panel with connected users
print_admin_panel(Clients) ->
  io:format("~n"),
  io:format("  ~s~n", [chat_ui:yellow("+===================================================+")]),
  io:format("  ~s~s~s~n", [chat_ui:yellow("|"), chat_ui:bold("              ADMIN: Connected Users              "), chat_ui:yellow("|")]),
  io:format("  ~s~n", [chat_ui:yellow("+===================================================+")]),
  io:format("  ~s~s~s~n", [chat_ui:yellow("|"), chat_ui:dim("  Status  Nickname        PID                     "), chat_ui:yellow("|")]),
  io:format("  ~s~n", [chat_ui:yellow("+---------------------------------------------------+")]),
  print_admin_users(Clients),
  io:format("  ~s~n", [chat_ui:yellow("+---------------------------------------------------+")]),
  TotalStr = "  Total: " ++ integer_to_list(length(Clients)) ++ " user(s)",
  Padding = 49 - length(TotalStr),
  io:format("  ~s~s~s~s~n", [chat_ui:yellow("|"), chat_ui:bold(TotalStr), lists:duplicate(Padding, $ ), chat_ui:yellow("|")]),
  io:format("  ~s~n~n", [chat_ui:yellow("+===================================================+")]).

print_admin_users([]) ->
  io:format("  ~s~s~s~n", [chat_ui:yellow("|"), chat_ui:dim("  (no users connected)                            "), chat_ui:yellow("|")]);
print_admin_users([{Nick, Pid} | T]) ->
  StatusDot = chat_ui:status_dot(online),
  PidStr = lists:flatten(io_lib:format("~p", [Pid])),
  NickPadded = string:pad(Nick, 14, trailing),
  PidPadded = string:pad(PidStr, 28, trailing),
  io:format("  ~s  ~s   ~s~s~s~n", [
    chat_ui:yellow("|"),
    StatusDot,
    chat_ui:cyan(NickPadded),
    chat_ui:dim(PidPadded),
    chat_ui:yellow("|")
  ]),
  print_admin_users(T).
