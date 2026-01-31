-module(chat_ui).
-export([
    %% Colors
    color/2, reset/0,
    green/1, red/1, yellow/1, cyan/1, magenta/1, blue/1, bold/1, dim/1,
    %% Formatting
    timestamp/0,
    %% Message formatting
    format_chat_msg/2, format_whisper/2, format_system/1, format_error/1,
    format_success/1, format_info/1, format_room_msg/3,
    %% Status
    status_dot/1
]).

%% ANSI color codes
-define(RESET,   "\e[0m").
-define(BOLD,    "\e[1m").
-define(DIM,     "\e[2m").
-define(RED,     "\e[31m").
-define(GREEN,   "\e[32m").
-define(YELLOW,  "\e[33m").
-define(BLUE,    "\e[34m").
-define(MAGENTA, "\e[35m").
-define(CYAN,    "\e[36m").

%% Color wrapper functions
reset() -> ?RESET.
color(Text, Color) -> Color ++ Text ++ ?RESET.

green(Text)   -> color(Text, ?GREEN).
red(Text)     -> color(Text, ?RED).
yellow(Text)  -> color(Text, ?YELLOW).
cyan(Text)    -> color(Text, ?CYAN).
magenta(Text) -> color(Text, ?MAGENTA).
blue(Text)    -> color(Text, ?BLUE).
bold(Text)    -> color(Text, ?BOLD).
dim(Text)     -> color(Text, ?DIM).

%% Timestamp
timestamp() ->
    {{_Y, _M, _D}, {Hour, Min, Sec}} = calendar:local_time(),
    lists:flatten(io_lib:format("~2..0B:~2..0B:~2..0B", [Hour, Min, Sec])).

%% Status dot
status_dot(online) -> green("*");
status_dot(offline) -> red("x").

%% Message formatting
format_chat_msg(Author, Message) ->
    TS = dim("[" ++ timestamp() ++ "] "),
    AuthorFormatted = cyan("[" ++ Author ++ "]"),
    io:format("~s~s ~s~n", [TS, AuthorFormatted, Message]).

format_whisper(Author, Message) ->
    TS = dim("[" ++ timestamp() ++ "] "),
    Prefix = magenta("(Whisper)"),
    AuthorFormatted = yellow("[" ++ Author ++ "]"),
    io:format("~s~s ~s ~s~n", [TS, Prefix, AuthorFormatted, Message]).

format_system(Message) ->
    TS = dim("[" ++ timestamp() ++ "] "),
    io:format("~s~s ~s~n", [TS, blue("[SYSTEM]"), Message]).

format_error(Message) ->
    TS = dim("[" ++ timestamp() ++ "] "),
    io:format("~s~s ~s~n", [TS, red("[ERROR]"), red(lists:flatten(Message))]).

format_success(Message) ->
    TS = dim("[" ++ timestamp() ++ "] "),
    io:format("~s~s ~s~n", [TS, green("[OK]"), green(lists:flatten(Message))]).

format_info(Message) ->
    TS = dim("[" ++ timestamp() ++ "] "),
    io:format("~s~s ~s~n", [TS, cyan("[INFO]"), lists:flatten(Message)]).

format_room_msg(RoomName, Author, Message) ->
    TS = dim("[" ++ timestamp() ++ "] "),
    RoomFormatted = yellow("[#" ++ RoomName ++ "]"),
    AuthorFormatted = cyan("[" ++ Author ++ "]"),
    io:format("~s~s ~s ~s~n", [TS, RoomFormatted, AuthorFormatted, Message]).
