-module(chat_listener_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Setup and Teardown Helpers
%% =============================================================================

setup() ->
    catch unregister(chat_server),
    chat_server:start().

teardown(ServerPid) ->
    exit(ServerPid, kill),
    timer:sleep(50).

%% =============================================================================
%% Client Connection Tests
%% =============================================================================

%% Note: chat_listener:start/2 expects to connect to a remote node,
%% which requires distributed Erlang. For unit tests, we test the
%% underlying message protocol directly.

client_say_command_test() ->
    ServerPid = setup(),

    %% Simulate a client process
    ServerPid ! {subscribe, self(), "TestClient"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Simulate say command (what chat_listener does)
    ServerPid ! {broadcast, self(), "Test message"},

    %% Should receive the message back (in general room)
    receive
        {room_msg, "general", "TestClient", "Test message"} -> ok
    after 1000 ->
        ?assert(false)
    end,

    teardown(ServerPid).

client_whisper_command_test() ->
    ServerPid = setup(),
    Self = self(),

    %% Create another "client"
    Receiver = spawn(fun() ->
        ServerPid ! {subscribe, self(), "Receiver"},
        receive {subscribe_result, ok} -> ok after 1000 -> ok end,
        receive
            {private_msg, "Sender", "Whisper message"} ->
                Self ! {test_result, received}
        after 2000 ->
            Self ! {test_result, timeout}
        end
    end),

    timer:sleep(100),

    %% Subscribe as sender
    ServerPid ! {subscribe, self(), "Sender"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Send whisper
    ServerPid ! {private_msg, self(), "Receiver", "Whisper message"},

    receive
        {test_result, received} -> ok;
        {test_result, timeout} -> ?assert(false)
    after 2000 ->
        ?assert(false)
    end,

    exit(Receiver, kill),
    teardown(ServerPid).

client_who_is_online_command_test() ->
    ServerPid = setup(),

    ServerPid ! {subscribe, self(), "Client1"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Request users list (what who_is_online does)
    ServerPid ! {get_users, self()},

    receive
        {users_list, Users} ->
            ?assert(lists:member("Client1", Users))
    after 1000 ->
        ?assert(false)
    end,

    teardown(ServerPid).

%% =============================================================================
%% Room Command Tests (via client simulation)
%% =============================================================================

client_create_room_command_test() ->
    ServerPid = setup(),

    ServerPid ! {subscribe, self(), "Client"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Simulate create_room command
    ServerPid ! {create_room, self(), "myroom"},

    receive
        {room_created, "myroom"} -> ok
    after 1000 ->
        ?assert(false)
    end,

    teardown(ServerPid).

client_join_room_command_test() ->
    ServerPid = setup(),

    ServerPid ! {subscribe, self(), "Client"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Create room first
    ServerPid ! {create_room, self(), "testroom"},
    receive {room_created, "testroom"} -> ok after 1000 -> ?assert(false) end,

    %% Leave it
    ServerPid ! {leave_room, self(), "testroom"},
    receive {room_left, "testroom"} -> ok after 1000 -> ?assert(false) end,

    %% Join again
    ServerPid ! {join_room, self(), "testroom"},

    receive
        {room_joined, "testroom"} -> ok
    after 1000 ->
        ?assert(false)
    end,

    teardown(ServerPid).

client_leave_room_command_test() ->
    ServerPid = setup(),

    ServerPid ! {subscribe, self(), "Client"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Leave general room
    ServerPid ! {leave_room, self(), "general"},

    receive
        {room_left, "general"} -> ok
    after 1000 ->
        ?assert(false)
    end,

    teardown(ServerPid).

client_room_say_command_test() ->
    ServerPid = setup(),

    ServerPid ! {subscribe, self(), "Client"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Send message to general room
    ServerPid ! {room_msg, self(), "general", "Room message"},

    receive
        {room_msg, "general", "Client", "Room message"} -> ok
    after 1000 ->
        ?assert(false)
    end,

    teardown(ServerPid).

client_list_rooms_command_test() ->
    ServerPid = setup(),

    ServerPid ! {subscribe, self(), "Client"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    ServerPid ! {get_rooms, self()},

    receive
        {rooms_list, Rooms} ->
            ?assert(lists:member("general", Rooms))
    after 1000 ->
        ?assert(false)
    end,

    teardown(ServerPid).

client_room_users_command_test() ->
    ServerPid = setup(),

    ServerPid ! {subscribe, self(), "Client"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    ServerPid ! {get_room_users, self(), "general"},

    receive
        {room_users_list, "general", Users} ->
            ?assert(lists:member("Client", Users))
    after 1000 ->
        ?assert(false)
    end,

    teardown(ServerPid).

%% =============================================================================
%% Multi-Client Scenarios
%% =============================================================================

multiple_clients_broadcast_test() ->
    ServerPid = setup(),
    Self = self(),

    %% Create second client
    Client2 = spawn(fun() ->
        ServerPid ! {subscribe, self(), "Client2"},
        receive {subscribe_result, ok} -> ok after 1000 -> ok end,
        %% Flush join notification from Client1
        receive {room_msg, _, _, _} -> ok after 100 -> ok end,
        receive
            {room_msg, "general", "Client1", "Hello all!"} ->
                Self ! {client2_received, ok}
        after 2000 ->
            Self ! {client2_received, timeout}
        end
    end),

    timer:sleep(100),

    %% Subscribe as Client1
    ServerPid ! {subscribe, self(), "Client1"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    timer:sleep(100),

    %% Client1 broadcasts
    ServerPid ! {broadcast, self(), "Hello all!"},

    %% Client1 should receive it too
    receive
        {room_msg, "general", "Client1", "Hello all!"} -> ok
    after 1000 ->
        ?assert(false)
    end,

    %% Verify Client2 received it
    receive
        {client2_received, ok} -> ok;
        {client2_received, timeout} -> ?assert(false)
    after 2000 ->
        ?assert(false)
    end,

    exit(Client2, kill),
    teardown(ServerPid).

room_message_isolation_test() ->
    ServerPid = setup(),
    Self = self(),

    %% Create Client2 who stays only in general
    Client2 = spawn(fun() ->
        ServerPid ! {subscribe, self(), "Client2"},
        receive {subscribe_result, ok} -> ok after 1000 -> ok end,
        %% Wait and check we don't receive erlang room message
        receive
            {room_msg, "erlang", _, _} ->
                Self ! {client2_result, received_erlang}
        after 1000 ->
            Self ! {client2_result, no_erlang_msg}
        end
    end),

    timer:sleep(100),

    %% Subscribe as Client1
    ServerPid ! {subscribe, self(), "Client1"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Client1 creates and joins erlang room
    ServerPid ! {create_room, self(), "erlang"},
    receive {room_created, "erlang"} -> ok after 1000 -> ?assert(false) end,

    %% Send message to erlang room
    ServerPid ! {room_msg, self(), "erlang", "Erlang specific"},

    %% Client1 should receive it
    receive
        {room_msg, "erlang", "Client1", "Erlang specific"} -> ok
    after 1000 ->
        ?assert(false)
    end,

    %% Client2 should NOT have received the erlang room message
    receive
        {client2_result, no_erlang_msg} -> ok;
        {client2_result, received_erlang} -> ?assert(false)
    after 2000 ->
        ?assert(false)
    end,

    exit(Client2, kill),
    teardown(ServerPid).

join_notification_test() ->
    ServerPid = setup(),

    %% First client
    ServerPid ! {subscribe, self(), "First"},
    receive {subscribe_result, ok} -> ok after 1000 -> ?assert(false) end,

    %% Spawn second client
    _Client2 = spawn(fun() ->
        timer:sleep(100),
        ServerPid ! {subscribe, self(), "Second"},
        receive {subscribe_result, _} -> ok after 1000 -> ok end,
        %% Keep alive briefly
        timer:sleep(500)
    end),

    %% First client should receive join notification
    receive
        {chat_msg, "SERVER", Msg} ->
            ?assert(string:find(Msg, "Second") =/= nomatch),
            ?assert(string:find(Msg, "joined") =/= nomatch)
    after 2000 ->
        ?assert(false)
    end,

    teardown(ServerPid).
