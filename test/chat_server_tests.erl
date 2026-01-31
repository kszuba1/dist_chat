-module(chat_server_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Room Helper Function Tests
%% =============================================================================

add_user_to_room_empty_room_test() ->
    Rooms = #{"general" => []},
    Result = chat_server:add_user_to_room("Anna", "general", Rooms),
    ?assertEqual(#{"general" => ["Anna"]}, Result).

add_user_to_room_existing_members_test() ->
    Rooms = #{"general" => ["Bob"]},
    Result = chat_server:add_user_to_room("Anna", "general", Rooms),
    ?assertEqual(#{"general" => ["Anna", "Bob"]}, Result).

add_user_to_room_already_member_test() ->
    Rooms = #{"general" => ["Anna", "Bob"]},
    Result = chat_server:add_user_to_room("Anna", "general", Rooms),
    ?assertEqual(#{"general" => ["Anna", "Bob"]}, Result).

add_user_to_nonexistent_room_test() ->
    Rooms = #{"general" => []},
    Result = chat_server:add_user_to_room("Anna", "newroom", Rooms),
    ?assertEqual(#{"general" => [], "newroom" => ["Anna"]}, Result).

remove_user_from_room_test() ->
    Rooms = #{"general" => ["Anna", "Bob"]},
    Result = chat_server:remove_user_from_room("Anna", "general", Rooms),
    ?assertEqual(#{"general" => ["Bob"]}, Result).

remove_user_from_room_last_member_test() ->
    Rooms = #{"general" => ["Anna"]},
    Result = chat_server:remove_user_from_room("Anna", "general", Rooms),
    ?assertEqual(#{"general" => []}, Result).

remove_user_not_in_room_test() ->
    Rooms = #{"general" => ["Bob"]},
    Result = chat_server:remove_user_from_room("Anna", "general", Rooms),
    ?assertEqual(#{"general" => ["Bob"]}, Result).

%% =============================================================================
%% Server Process Tests
%% =============================================================================

server_start_test() ->
    %% Unregister if already registered from previous test
    catch unregister(chat_server),
    Pid = chat_server:start(),
    ?assert(is_pid(Pid)),
    ?assertEqual(Pid, whereis(chat_server)),
    exit(Pid, kill),
    timer:sleep(50).

subscribe_new_user_test() ->
    catch unregister(chat_server),
    ServerPid = chat_server:start(),

    ServerPid ! {subscribe, self(), "TestUser"},
    receive
        {subscribe_result, ok} -> ok
    after 1000 ->
        ?assert(false)
    end,

    exit(ServerPid, kill),
    timer:sleep(50).

subscribe_duplicate_nick_test() ->
    catch unregister(chat_server),
    ServerPid = chat_server:start(),

    %% First user subscribes
    ServerPid ! {subscribe, self(), "TestUser"},
    receive
        {subscribe_result, ok} -> ok
    after 1000 ->
        ?assert(false)
    end,

    %% Second user tries same nick
    ServerPid ! {subscribe, self(), "TestUser"},
    receive
        {subscribe_result, error, "Nick taken"} -> ok
    after 1000 ->
        ?assert(false)
    end,

    exit(ServerPid, kill),
    timer:sleep(50).

get_users_test() ->
    catch unregister(chat_server),
    ServerPid = chat_server:start(),

    %% Subscribe a user
    ServerPid ! {subscribe, self(), "Anna"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Request users list
    ServerPid ! {get_users, self()},
    receive
        {users_list, Nicks} ->
            ?assertEqual(["Anna"], Nicks)
    after 1000 ->
        ?assert(false)
    end,

    exit(ServerPid, kill),
    timer:sleep(50).

%% =============================================================================
%% Room Management Tests
%% =============================================================================

create_room_test() ->
    catch unregister(chat_server),
    ServerPid = chat_server:start(),

    %% Subscribe first
    ServerPid ! {subscribe, self(), "Anna"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Create room
    ServerPid ! {create_room, self(), "erlang"},
    receive
        {room_created, "erlang"} -> ok
    after 1000 ->
        ?assert(false)
    end,

    exit(ServerPid, kill),
    timer:sleep(50).

create_duplicate_room_test() ->
    catch unregister(chat_server),
    ServerPid = chat_server:start(),

    ServerPid ! {subscribe, self(), "Anna"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Create room
    ServerPid ! {create_room, self(), "erlang"},
    receive {room_created, "erlang"} -> ok after 1000 -> ?assert(false) end,

    %% Try to create same room again
    ServerPid ! {create_room, self(), "erlang"},
    receive
        {room_error, Msg} ->
            ?assert(string:find(Msg, "already exists") =/= nomatch)
    after 1000 ->
        ?assert(false)
    end,

    exit(ServerPid, kill),
    timer:sleep(50).

join_room_test() ->
    catch unregister(chat_server),
    ServerPid = chat_server:start(),

    ServerPid ! {subscribe, self(), "Anna"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% User is auto-joined to general, try joining again
    ServerPid ! {join_room, self(), "general"},
    receive
        {room_error, Msg} ->
            ?assert(string:find(Msg, "already in room") =/= nomatch)
    after 1000 ->
        ?assert(false)
    end,

    exit(ServerPid, kill),
    timer:sleep(50).

join_nonexistent_room_test() ->
    catch unregister(chat_server),
    ServerPid = chat_server:start(),

    ServerPid ! {subscribe, self(), "Anna"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    ServerPid ! {join_room, self(), "nonexistent"},
    receive
        {room_error, Msg} ->
            ?assert(string:find(Msg, "does not exist") =/= nomatch)
    after 1000 ->
        ?assert(false)
    end,

    exit(ServerPid, kill),
    timer:sleep(50).

leave_room_test() ->
    catch unregister(chat_server),
    ServerPid = chat_server:start(),

    ServerPid ! {subscribe, self(), "Anna"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Leave general room
    ServerPid ! {leave_room, self(), "general"},
    receive
        {room_left, "general"} -> ok
    after 1000 ->
        ?assert(false)
    end,

    exit(ServerPid, kill),
    timer:sleep(50).

leave_room_not_member_test() ->
    catch unregister(chat_server),
    ServerPid = chat_server:start(),

    ServerPid ! {subscribe, self(), "Anna"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Create a room but don't join (we auto-join on create, so leave first)
    ServerPid ! {create_room, self(), "erlang"},
    receive {room_created, "erlang"} -> ok after 1000 -> ?assert(false) end,

    ServerPid ! {leave_room, self(), "erlang"},
    receive {room_left, "erlang"} -> ok after 1000 -> ?assert(false) end,

    %% Try to leave again
    ServerPid ! {leave_room, self(), "erlang"},
    receive
        {room_error, Msg} ->
            ?assert(string:find(Msg, "not in room") =/= nomatch)
    after 1000 ->
        ?assert(false)
    end,

    exit(ServerPid, kill),
    timer:sleep(50).

get_rooms_test() ->
    catch unregister(chat_server),
    ServerPid = chat_server:start(),

    ServerPid ! {subscribe, self(), "Anna"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Get rooms (should have "general" by default)
    ServerPid ! {get_rooms, self()},
    receive
        {rooms_list, Rooms} ->
            ?assert(lists:member("general", Rooms))
    after 1000 ->
        ?assert(false)
    end,

    exit(ServerPid, kill),
    timer:sleep(50).

get_room_users_test() ->
    catch unregister(chat_server),
    ServerPid = chat_server:start(),

    ServerPid ! {subscribe, self(), "Anna"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Get room users
    ServerPid ! {get_room_users, self(), "general"},
    receive
        {room_users_list, "general", Users} ->
            ?assertEqual(["Anna"], Users)
    after 1000 ->
        ?assert(false)
    end,

    exit(ServerPid, kill),
    timer:sleep(50).

get_room_users_nonexistent_test() ->
    catch unregister(chat_server),
    ServerPid = chat_server:start(),

    ServerPid ! {subscribe, self(), "Anna"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    ServerPid ! {get_room_users, self(), "nonexistent"},
    receive
        {room_error, Msg} ->
            ?assert(string:find(Msg, "does not exist") =/= nomatch)
    after 1000 ->
        ?assert(false)
    end,

    exit(ServerPid, kill),
    timer:sleep(50).

room_message_test() ->
    catch unregister(chat_server),
    ServerPid = chat_server:start(),

    ServerPid ! {subscribe, self(), "Anna"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Send message to general room
    ServerPid ! {room_msg, self(), "general", "Hello!"},
    receive
        {room_msg, "general", "Anna", "Hello!"} -> ok
    after 1000 ->
        ?assert(false)
    end,

    exit(ServerPid, kill),
    timer:sleep(50).

room_message_not_member_test() ->
    catch unregister(chat_server),
    ServerPid = chat_server:start(),

    ServerPid ! {subscribe, self(), "Anna"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Create and leave a room
    ServerPid ! {create_room, self(), "erlang"},
    receive {room_created, "erlang"} -> ok after 1000 -> ?assert(false) end,

    ServerPid ! {leave_room, self(), "erlang"},
    receive {room_left, "erlang"} -> ok after 1000 -> ?assert(false) end,

    %% Try to send message to room we're not in
    ServerPid ! {room_msg, self(), "erlang", "Hello!"},
    receive
        {room_error, Msg} ->
            ?assert(string:find(Msg, "not in room") =/= nomatch)
    after 1000 ->
        ?assert(false)
    end,

    exit(ServerPid, kill),
    timer:sleep(50).

%% =============================================================================
%% Broadcast (Backward Compatibility) Tests
%% =============================================================================

broadcast_to_general_test() ->
    catch unregister(chat_server),
    ServerPid = chat_server:start(),

    ServerPid ! {subscribe, self(), "Anna"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Broadcast should go to general room
    ServerPid ! {broadcast, self(), "Hello everyone!"},
    receive
        {room_msg, "general", "Anna", "Hello everyone!"} -> ok
    after 1000 ->
        ?assert(false)
    end,

    exit(ServerPid, kill),
    timer:sleep(50).

%% =============================================================================
%% Private Message Tests
%% =============================================================================

private_message_test() ->
    catch unregister(chat_server),
    ServerPid = chat_server:start(),

    %% We need two "users" - use spawn for second one
    Self = self(),
    Receiver = spawn(fun() ->
        ServerPid ! {subscribe, self(), "Bob"},
        receive {subscribe_result, ok} -> ok after 1000 -> ok end,
        receive
            {private_msg, "Anna", "Secret"} ->
                Self ! {test_result, ok}
        after 2000 ->
            Self ! {test_result, timeout}
        end
    end),

    timer:sleep(100),

    ServerPid ! {subscribe, self(), "Anna"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Send private message
    ServerPid ! {private_msg, self(), "Bob", "Secret"},

    receive
        {test_result, ok} -> ok;
        {test_result, timeout} -> ?assert(false)
    after 2000 ->
        ?assert(false)
    end,

    exit(Receiver, kill),
    exit(ServerPid, kill),
    timer:sleep(50).

private_message_user_not_found_test() ->
    catch unregister(chat_server),
    ServerPid = chat_server:start(),

    ServerPid ! {subscribe, self(), "Anna"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Send to nonexistent user
    ServerPid ! {private_msg, self(), "Nobody", "Hello"},
    receive
        {chat_msg, "SERVER", Msg} ->
            ?assert(string:find(Msg, "not found") =/= nomatch)
    after 1000 ->
        ?assert(false)
    end,

    exit(ServerPid, kill),
    timer:sleep(50).
