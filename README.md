# Erlang Distributed Chat

A **distributed chat system** written in **Erlang/OTP**, built and managed using **Rebar3**. Features colorful CLI output with timestamps, box-drawn UI elements, and real-time messaging.

## Requirements

- **Erlang/OTP** >= 25
- **Rebar3** >= 3.20

## Project Structure

```
dist_chat/
├── src/
│   ├── chat_server.erl    # Server: manages users, routes messages
│   ├── chat_listener.erl  # Client: connects to server, sends/receives messages
│   ├── chat_ui.erl        # UI helpers: colors, formatting, timestamps
│   ├── dist_chat_app.erl  # OTP application module
│   ├── dist_chat_sup.erl  # OTP supervisor
│   └── dist_chat.app.src  # Application config
└── rebar.config           # Build configuration
```

## Features

| Feature | Description |
|---------|-------------|
| **Chat Rooms** | Create and join multiple rooms, messages are delivered only to room members |
| **Broadcast Messages** | Send messages to all users in a room |
| **Whisper (Private Messages)** | Send private messages to specific users |
| **Online Users List** | Check who is currently online |
| **Nicknames** | Users connect with unique nicknames |
| **Join Notifications** | All users are notified when someone joins |
| **Colorful CLI** | ANSI colors for different message types |
| **Timestamps** | All messages display `[HH:MM:SS]` timestamps |
| **Box-drawn UI** | Unicode box characters for clean formatting |

## How to Run

### 1. Compile the Project

```bash
rebar3 compile
```

### 2. Start the Server

Open a terminal and run:

```bash
erl -pa _build/default/lib/dist_chat/ebin -sname server@localhost -setcookie mycookie
```

In the Erlang shell:

```erlang
chat_server:start().
```

Output:
```
  +=============================================+
  |        DISTRIBUTED CHAT SERVER             |
  |            Erlang/OTP Edition              |
  +=============================================+

[10:30:15] [INFO] Initializing chat server...
[10:30:15] [OK] Server started (PID: <0.123.0>)
[10:30:15] [INFO] Default room 'general' created
```

### 3. Start a Client

Open another terminal and run:

```bash
erl -pa _build/default/lib/dist_chat/ebin -sname client1@localhost -setcookie mycookie
```

In the Erlang shell, connect with a nickname:

```erlang
P = chat_listener:start('server@localhost', "Anna").
```

Output:
```
[10:31:00] [INFO] Connecting to chat server...
[10:31:00] [OK] Successfully joined as 'Anna'

  +=============================================+
  |       Welcome to Distributed Chat!          |
  +=============================================+
  |  General Commands:                          |
  |    Pid ! {say, "message"}                   |
  |    Pid ! {whisper, "nick", "msg"}           |
  |    Pid ! who_is_online                      |
  +---------------------------------------------+
  |  Room Commands:                             |
  |    Pid ! {create_room, "name"}              |
  |    Pid ! {join_room, "name"}                |
  |    Pid ! {leave_room, "name"}               |
  |    Pid ! {room_say, "name", "msg"}          |
  |    Pid ! list_rooms                         |
  |    Pid ! {room_users, "name"}               |
  +=============================================+
  Your nickname: Anna
  Auto-joined room: general
```

### 4. Send a Broadcast Message

Send a message to all users in the "general" room (default):

```erlang
P ! {say, "Hello everyone!"}.
```

Output (all clients in "general" room see):
```
[10:32:15] [#general] [Anna] Hello everyone!
```

### 5. Send a Whisper (Private Message)

Send a private message to a specific user:

```erlang
P ! {whisper, "Tomek", "This is a secret message!"}.
```

Output (only Tomek sees):
```
[10:33:20] (Whisper) [Anna]: This is a secret message!
```

### 6. Check Who is Online (Client)

```erlang
P ! who_is_online.
```

Output:
```
  +-----------------------------+
  |       Online Users          |
  +-----------------------------+
  | * Anna                      |
  | * Tomek                     |
  | * Maria                     |
  +-----------------------------+
  Total: 3 user(s)
```

### 7. Create a Room

Create a new chat room (you automatically join it):

```erlang
P ! {create_room, "erlang"}.
```

Output:
```
[10:34:00] [OK] Room 'erlang' created and joined
```

### 8. Join an Existing Room

```erlang
P ! {join_room, "erlang"}.
```

Output:
```
[10:34:30] [OK] Joined room 'erlang'
```

### 9. Send a Message to a Room

```erlang
P ! {room_say, "erlang", "Hello Erlang fans!"}.
```

Output (all room members see):
```
[10:35:00] [#erlang] [Anna] Hello Erlang fans!
```

### 10. List All Rooms

```erlang
P ! list_rooms.
```

Output:
```
  +-----------------------------+
  |       Available Rooms       |
  +-----------------------------+
  | #general                    |
  | #erlang                     |
  +-----------------------------+
  Total: 2 room(s)
```

### 11. List Room Members

```erlang
P ! {room_users, "erlang"}.
```

Output:
```
  +-----------------------------+
  |  Room: #erlang              |
  +-----------------------------+
  | * Anna                      |
  | * Tomek                     |
  +-----------------------------+
  Members: 2
```

### 12. Leave a Room

```erlang
P ! {leave_room, "erlang"}.
```

Output:
```
[10:36:00] [INFO] Left room 'erlang'
```

### 13. Admin: View Connected Users (Server)

On the server terminal:

```erlang
chat_server:print_users().
```

Output:
```
  +===================================================+
  |              ADMIN: Connected Users               |
  +===================================================+
  |  Status  Nickname        PID                      |
  +---------------------------------------------------+
  |  *   Anna           <0.89.0>                      |
  |  *   Tomek          <0.92.0>                      |
  +---------------------------------------------------+
  |  Total: 2 user(s)                                 |
  +===================================================+
```

## Command Reference

### Server Commands

| Command | Description |
|---------|-------------|
| `chat_server:start()` | Start the chat server |
| `chat_server:print_users()` | Admin view of all connected users |

### Client Commands

| Command | Description |
|---------|-------------|
| `chat_listener:start(ServerNode, Nick)` | Connect to server with nickname |
| `Pid ! {say, Message}` | Broadcast message to "general" room |
| `Pid ! {whisper, TargetNick, Message}` | Send private message |
| `Pid ! who_is_online` | List online users |

### Room Commands

| Command | Description |
|---------|-------------|
| `Pid ! {create_room, RoomName}` | Create a new room (auto-joins) |
| `Pid ! {join_room, RoomName}` | Join an existing room |
| `Pid ! {leave_room, RoomName}` | Leave a room |
| `Pid ! {room_say, RoomName, Message}` | Send message to a specific room |
| `Pid ! list_rooms` | List all available rooms |
| `Pid ! {room_users, RoomName}` | List members of a room |

## Visual Elements

The CLI uses ANSI colors for better readability:

- **Green**: Success messages, online status indicators
- **Cyan**: Usernames, info messages, user list box borders
- **Yellow**: Room names (e.g., `[#general]`), room list box, admin panel
- **Magenta**: Server banner, whisper styling, room members box
- **Red**: Error messages
- **Dim gray**: Timestamps, secondary info

## Multiple Clients Example

To test with multiple clients, open additional terminals:

```bash
# Terminal 3
erl -pa _build/default/lib/dist_chat/ebin -sname client2@localhost -setcookie mycookie
```

```erlang
P2 = chat_listener:start('server@localhost', "Tomek").
```

```bash
# Terminal 4
erl -pa _build/default/lib/dist_chat/ebin -sname client3@localhost -setcookie mycookie
```

```erlang
P3 = chat_listener:start('server@localhost', "Maria").
```

All clients will receive join notifications when new users connect.
