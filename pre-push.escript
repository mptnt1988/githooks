#!/usr/bin/env escript

-include_lib("kernel/include/file.hrl").

%%==============================================================================
%% MAIN
%%==============================================================================

main([Remote, Url]) ->
    add_lib_code_paths(),
    case io:fread("", "~s ~s ~s ~s") of
        {ok, [LocalRef, LocalSHA, RemoteRef, RemoteSHA]} ->

            %%------------------------------------------------------------------
            %% Get OS Session ID
            io:format("OS Session ID: ~p~n", [[{?MODULE, ?LINE},
                                               git:get_os_session_id()]]),

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
            Data = collect_git_data(BasicData),
            io:format("Git: ~p~n", [[{?MODULE, ?LINE}, Data]]),
            ok = git:write_info(Data),

            %%------------------------------------------------------------------
            %% Test gerrit module - review command
            Res = gerrit:review(#{host => "gerrit.server.com",
                                  port => "29418",
                                  project => "gerrit/project-name"},
                                [{verified, "+1"}, {codereview, "+2"}, submit],
                                LocalSHA),
            io:format("Gerrit: ~p~n", [[{?MODULE, ?LINE}, Res]]),

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
            end;
        _Errors ->
            halt(1)
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

collect_git_data(BasicData) when is_map(BasicData) ->
    GitData = git:collect_data(),
    maps:merge(BasicData, GitData).

is_symlink(FileName) ->
    {ok, LinkInfo} = file:read_link_info(FileName),
    LinkInfo#file_info.type == symlink.

get_script_path() ->
    ScriptPath = escript:script_name(),
    case is_symlink(ScriptPath) of
        true ->
            GetSrcCmd = "readlink -f " ++ ScriptPath,
            string:trim(os:cmd(GetSrcCmd));
        false ->
            ScriptPath
    end.

add_lib_code_paths() ->
    ScriptPath = get_script_path(),
    ScriptDirName = filename:dirname(ScriptPath),
    GitLibRelatedPath = "libs/erlang/git/_build/default/lib/git/ebin",
    GitLibFullPath = filename:join(ScriptDirName, GitLibRelatedPath),
    true = code:add_patha(GitLibFullPath).
