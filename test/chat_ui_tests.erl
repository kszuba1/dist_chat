-module(chat_ui_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Color Function Tests
%% =============================================================================

reset_test() ->
    ?assertEqual("\e[0m", chat_ui:reset()).

green_test() ->
    Result = chat_ui:green("test"),
    ?assert(string:find(Result, "\e[32m") =/= nomatch),
    ?assert(string:find(Result, "test") =/= nomatch),
    ?assert(string:find(Result, "\e[0m") =/= nomatch).

red_test() ->
    Result = chat_ui:red("error"),
    ?assert(string:find(Result, "\e[31m") =/= nomatch),
    ?assert(string:find(Result, "error") =/= nomatch).

yellow_test() ->
    Result = chat_ui:yellow("warning"),
    ?assert(string:find(Result, "\e[33m") =/= nomatch),
    ?assert(string:find(Result, "warning") =/= nomatch).

cyan_test() ->
    Result = chat_ui:cyan("info"),
    ?assert(string:find(Result, "\e[36m") =/= nomatch),
    ?assert(string:find(Result, "info") =/= nomatch).

magenta_test() ->
    Result = chat_ui:magenta("special"),
    ?assert(string:find(Result, "\e[35m") =/= nomatch),
    ?assert(string:find(Result, "special") =/= nomatch).

blue_test() ->
    Result = chat_ui:blue("system"),
    ?assert(string:find(Result, "\e[34m") =/= nomatch),
    ?assert(string:find(Result, "system") =/= nomatch).

bold_test() ->
    Result = chat_ui:bold("important"),
    ?assert(string:find(Result, "\e[1m") =/= nomatch),
    ?assert(string:find(Result, "important") =/= nomatch).

dim_test() ->
    Result = chat_ui:dim("subtle"),
    ?assert(string:find(Result, "\e[2m") =/= nomatch),
    ?assert(string:find(Result, "subtle") =/= nomatch).

color_with_custom_test() ->
    Result = chat_ui:color("text", "\e[31m"),
    ?assertEqual("\e[31mtext\e[0m", Result).

%% =============================================================================
%% Timestamp Tests
%% =============================================================================

timestamp_format_test() ->
    TS = chat_ui:timestamp(),
    %% Should be in format HH:MM:SS
    ?assertEqual(8, length(TS)),
    ?assertEqual($:, lists:nth(3, TS)),
    ?assertEqual($:, lists:nth(6, TS)).

timestamp_valid_numbers_test() ->
    TS = chat_ui:timestamp(),
    [H1, H2, $:, M1, M2, $:, S1, S2] = TS,
    %% All should be digits
    ?assert(H1 >= $0 andalso H1 =< $9),
    ?assert(H2 >= $0 andalso H2 =< $9),
    ?assert(M1 >= $0 andalso M1 =< $9),
    ?assert(M2 >= $0 andalso M2 =< $9),
    ?assert(S1 >= $0 andalso S1 =< $9),
    ?assert(S2 >= $0 andalso S2 =< $9).

%% =============================================================================
%% Status Dot Tests
%% =============================================================================

status_dot_online_test() ->
    Result = chat_ui:status_dot(online),
    %% Should contain green color and asterisk
    ?assert(string:find(Result, "\e[32m") =/= nomatch),
    ?assert(string:find(Result, "*") =/= nomatch).

status_dot_offline_test() ->
    Result = chat_ui:status_dot(offline),
    %% Should contain red color and x
    ?assert(string:find(Result, "\e[31m") =/= nomatch),
    ?assert(string:find(Result, "x") =/= nomatch).

%% =============================================================================
%% Message Formatting Output Tests
%% These tests verify the functions don't crash - actual output goes to stdout
%% =============================================================================

format_chat_msg_no_crash_test() ->
    %% Capture output to verify no crash
    ?assertEqual(ok, chat_ui:format_chat_msg("Anna", "Hello")).

format_whisper_no_crash_test() ->
    ?assertEqual(ok, chat_ui:format_whisper("Bob", "Secret message")).

format_system_no_crash_test() ->
    ?assertEqual(ok, chat_ui:format_system("System notification")).

format_error_no_crash_test() ->
    ?assertEqual(ok, chat_ui:format_error("An error occurred")).

format_success_no_crash_test() ->
    ?assertEqual(ok, chat_ui:format_success("Operation successful")).

format_info_no_crash_test() ->
    ?assertEqual(ok, chat_ui:format_info("Information message")).

format_room_msg_no_crash_test() ->
    ?assertEqual(ok, chat_ui:format_room_msg("general", "Anna", "Hello room")).

%% =============================================================================
%% Edge Cases
%% =============================================================================

empty_string_color_test() ->
    Result = chat_ui:green(""),
    ?assertEqual("\e[32m\e[0m", Result).

special_characters_test() ->
    Result = chat_ui:cyan("Hello\nWorld\t!"),
    ?assert(string:find(Result, "Hello\nWorld\t!") =/= nomatch).

unicode_characters_test() ->
    Result = chat_ui:yellow("Cześć świecie!"),
    ?assert(string:find(Result, "Cześć świecie!") =/= nomatch).
