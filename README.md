# Erlang Distributed Chat
A simple **distributed chat system** written in **Erlang**, built and managed using **Rebar3**.

## 🚀 Requirements
You’ll need the following installed:

- **Erlang/OTP** ≥ 25
- **Rebar3** ≥ 3.20

## How to run
1. Compile the Project.  
in the root directory of the project run:  
`rebar3 compile`
2. Start the Server in a new terminal window.  
`erl -pa _build/default/lib/dist_chat/ebin -sname node1@localhost -setcookie mycookie`  
`chat_server:start().` 
3. Start the Client with a nickname in another terminal window.  
`erl -pa _build/default/lib/dist_chat/ebin -sname node2@localhost -setcookie mycookie`  
`P = chat_listener:start('node1@localhost', "Anna").`
4. Send a message from the Client to the Server.  
`P ! {say, "Hello everyone!"}.` 
5. Client checks who is online
`P ! who_is_online.`
6. Server checks who is online
`chat_server:print_users().`