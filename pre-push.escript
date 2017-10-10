#!/usr/bin/env escript

%%==============================================================================
%% MAIN
%%==============================================================================

main([Remote, Url]) ->
    %% This is required to invoke git module
    add_lib_code_paths(),
    try io:fread("", "~s ~s ~s ~s") of
        {ok, [LocalRef, LocalSHA, RemoteRef, RemoteSHA]} ->

            %%------------------------------------------------------------------
            %% Print current working directory
            io:format("CWD: ~p~n", [file:get_cwd()]),

            %%------------------------------------------------------------------
            %% Print all arguments
            io:format("All arguments: ~p~n",
                      [[{Remote, Url},
                        {LocalRef, LocalSHA, RemoteRef, RemoteSHA}]]),

            %%------------------------------------------------------------------
            %% Test git module - write info
            BasicData = #{remote => Remote, url => Url,
                          local_ref => LocalRef, local_sha => LocalSHA,
                          remote_ref => RemoteRef, remote_sha => RemoteSHA},
            MoreGitData = git:collect_data(),
            Data = maps:merge(BasicData, MoreGitData),
            io:format("Git: ~p~n", [Data]),
            ok = git:write_info(Data),

            %%------------------------------------------------------------------
            %% Test git module - save and load data & user interaction
            BasicData = #{remote => Remote, url => Url,
                          local_ref => LocalRef, local_sha => LocalSHA,
                          remote_ref => RemoteRef, remote_sha => RemoteSHA},
            MoreGitData = git:collect_data(),
            Data = maps:merge(BasicData, MoreGitData),
            ok = git:save_data(Data),
            io:format("Save data to node successfully.~n"),

            %% User interaction
            process_flag(trap_exit, true),
            Port = open_port("/dev/tty", [eof]),
            io:format("UI: Port open ~p~n", [Port]),
            io:format("UI: Do you want to remove saved data (yes/...)? "),
            UIFun =
                fun UILoop() ->
                        receive
                            {Port, {data, PortData}} ->
                                case string:trim(PortData) of
                                    "yes" ->
                                        io:format("UI: got yes. Exit node.~n"),
                                        [exit];
                                    _ ->
                                        io:format("UI: got other than yes. "
                                                  "Keep node alive.~n"),
                                        []
                                end;
                            {'EXIT', Port, Reason} ->
                                io:format("UI error: ~p~n", [Reason]),
                                git:node_stop(),
                                halt(1);
                            Unexpected ->
                                io:format("UI unexpected: ~p~n", [Unexpected]),
                                UILoop()
                        end
                end,
            UIOptions = UIFun(),

            LoadedData = git:load_data(UIOptions),
            io:format("Load data from node successfully: ~p~n", [LoadedData]),

            %%------------------------------------------------------------------
            %% Test gerrit module - review command
            Pid = self(),
            GerritFun =
                fun() ->
                        Settings = #{host => "gerrit.server.com",
                                     port => "29418",
                                     project => "gerrit/project-name"},
                        Actions = [{verified, "+1"},
                                   {codereview, "+2"},
                                   submit],
                        Pid ! {self(),
                               gerrit:review(Settings, Actions, LocalSHA)}
                end,
            process_flag(trap_exit, true),
            GerritPid = spawn_link(GerritFun),
            receive
                {GerritPid, GerritResult} ->
                    io:format("Gerrit: ~p~n", [GerritResult])
            after 2000 ->
                    io:format("Gerrit: FAILED TO REVIEW~n")
            end
    catch
        T:E ->
            io:format("Error:~n~p~n~p~n", [T, E]),
            io:format("Stracktrace:~n~p~n", [erlang:get_stacktrace()]),
            halt(1)
    end.

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
