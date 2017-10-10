#!/usr/bin/env escript

%%==============================================================================
%% MAIN
%%==============================================================================

main(_) ->
    %% This is required to invoke git module
    add_lib_code_paths(),
    LoadedData = git:load_data([exit]),
    io:format("Load data from node successfully: ~p~n", [LoadedData]).

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% This script may be symlinked, then escript:script_name() returns path to the
%% symlink. Get regular file path and dir of the script, then add dir of git lib
%% module to code paths.
%%------------------------------------------------------------------------------
add_lib_code_paths() ->
    ExePath = escript:script_name(),
    GetPathCmd = "readlink -f " ++ ExePath,
    ScriptPath = string:trim(os:cmd(GetPathCmd)),
    ScriptDir = filename:dirname(ScriptPath),
    GitLibPath = "libs/erlang/git/_build/default/lib/git/ebin",
    GitLibFullPath = filename:join(ScriptDir, GitLibPath),
    true = code:add_patha(GitLibFullPath).
