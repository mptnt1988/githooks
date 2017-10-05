#!/usr/bin/env escript

-include_lib("kernel/include/file.hrl").
-define(GITLIB_PATH, "libs/erlang/git/_build/default/lib/git/ebin").
-define(STARTNODE_SCRIPT, "support/start-erl.sh").
-define(STOPNODE_SCRIPT, "support/stop-erl.sh").

%%==============================================================================
%% MAIN
%%==============================================================================

main([Remote, Url]) ->
    add_lib_code_paths(),
    try io:fread("", "~s ~s ~s ~s") of
        {ok, [LocalRef, LocalSHA, RemoteRef, RemoteSHA]} ->

            %%------------------------------------------------------------------
            %% Get OS Session ID
            ScriptPID = os:getpid(),
            SessionID = git:get_os_pid(ScriptPID, "sid"),
            io:format("OS PID: ~p~n", [[{escript_pid, ScriptPID},
                                        {session_id, SessionID}]]),

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
            %% Start erl node
            ScriptDir = get_script_dir(),
            StartErlScript = filename:join(ScriptDir, ?STARTNODE_SCRIPT),
            ScriptPID = os:getpid(),
            SessionID = git:get_os_pid(ScriptPID, "sid"),
            NodeName = "githooks_" ++ SessionID,
            CodePaths = [filename:join(get_script_dir(), ?GITLIB_PATH)],
            Commands = ["-s git start_intermediary"],
            StartResult = git:node_start(StartErlScript, NodeName,
                                         CodePaths, Commands),
            io:format("Start node cmd: ~p~n", [StartResult]),

            %%------------------------------------------------------------------
            %% Check erl node status
            ScriptPID = os:getpid(),
            SessionID = git:get_os_pid(ScriptPID, "sid"),
            NodeName = "githooks_" ++ SessionID,
            StatusResult = git:node_status(NodeName),
            io:format("Node status: ~p~n", [StatusResult]),

            %%------------------------------------------------------------------
            %% Push data to erl node
            BasicData = #{remote => Remote, url => Url,
                          local_ref => LocalRef, local_sha => LocalSHA,
                          remote_ref => RemoteRef, remote_sha => RemoteSHA},
            MoreGitData = git:collect_data(),
            Data = maps:merge(BasicData, MoreGitData),
            ScriptPID = os:getpid(),
            SessionID = git:get_os_pid(ScriptPID, "sid"),
            NodeName = "githooks_" ++ SessionID,
            ok = git:call(NodeName, git, ipush, [Data]),
            io:format("Push data to node successfully.~n"),

            %%------------------------------------------------------------------
            %% Fetch data from erl node
            ScriptPID = os:getpid(),
            SessionID = git:get_os_pid(ScriptPID, "sid"),
            NodeName = "githooks_" ++ SessionID,
            {ok, FetchedData} = git:call(NodeName, git, ifetch, []),
            io:format("Fetch data from node successfully: ~p~n", [FetchedData]),

            %%------------------------------------------------------------------
            %% Stop erl node
            ScriptDir = get_script_dir(),
            StopErlScript = filename:join(ScriptDir, ?STOPNODE_SCRIPT),
            ScriptPID = os:getpid(),
            SessionID = git:get_os_pid(ScriptPID, "sid"),
            NodeName = "githooks_" ++ SessionID,
            StopResult = git:node_stop(StopErlScript, NodeName),
            io:format("Stop node cmd: ~p~n", [StopResult]),

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
            end,

            %%------------------------------------------------------------------
            %% Test user interaction
            process_flag(trap_exit, true),
            Port = open_port("/dev/tty", [eof]),
            io:format("Do you want to continue (yes/...)? "),
            receive
                {Port, {data, PortData}} ->
                    case string:trim(PortData) of
                        "yes" ->
                            io:format("Tuan got: ~p~n", [PortData]);
                        _ ->
                            io:format("Tuan exit: ~p~n", [PortData]),
                            halt(1)
                    end;
                {'EXIT', Port, Reason} ->
                    io:format("Tuan error: ~p~n", [Reason]),
                    halt(1);
                Unexpected ->
                    io:format("Tuan unexpected: ~p~n", [Unexpected])
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

add_lib_code_paths() ->
    ScriptDir = get_script_dir(),
    GitLibFullPath = filename:join(ScriptDir, ?GITLIB_PATH),
    true = code:add_patha(GitLibFullPath).

get_script_dir() ->
    ScriptPath = get_script_path(),
    filename:dirname(ScriptPath).

get_script_path() ->
    ScriptPath = escript:script_name(),
    case is_symlink(ScriptPath) of
        true ->
            GetSrcCmd = "readlink -f " ++ ScriptPath,
            string:trim(os:cmd(GetSrcCmd));
        false ->
            ScriptPath
    end.

is_symlink(FileName) ->
    {ok, LinkInfo} = file:read_link_info(FileName),
    LinkInfo#file_info.type == symlink.
