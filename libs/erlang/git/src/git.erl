-module(git).

%% API exports
-export([load/0,
         write_info/1,
         write_info/2,
         collect_data/0,
         ipush/1,
         ifetch/0,
         save_data/1,
         load_data/0,
         load_data/1
        ]).

-define(ERLGIT_SCRIPT, "support/erlang_git.sh").
-define(GITLIB_PATH, "libs/erlang/git/_build/default/lib/git/ebin").
-define(DEPS_PATHS, ["libs/erlang/git/_build/default/lib/mpgit/ebin",
                     "libs/erlang/git/_build/default/lib/lager/ebin"
                    ]).

-define(DATAPROC, intermediary).

-define(DBG_HEADER(X), get_node_name() ++ ": " ++ X).
-define(DBG(X), lager:debug(?DBG_HEADER(X))).
-define(DBG(X, Y), lager:debug(?DBG_HEADER(X), Y)).

%%==============================================================================
%% API functions
%%==============================================================================
load() ->
    [true = code:add_path(get_full_path(Path)) || Path <- ?DEPS_PATHS].

write_info(Data) when is_map(Data) ->
    {{Y, M, D}, {H, Ms, S}} = erlang:localtime(),
    TimeStampFormat = "~4w~.2.0w~.2.0w~.2.0w~.2.0w~.2.0w",
    TimeStamp = lists:flatten(io_lib:format(TimeStampFormat,
                                            [Y, M, D, H, Ms, S])),
    OutFile = "/tmp/git_" ++ TimeStamp,
    write_info(Data, OutFile).

write_info(Data, OutFile) when is_map(Data), is_list(OutFile) ->
    file:write_file(OutFile, io_lib:format("~p.~n", [Data])).

collect_data() ->
    #{
       current_branch => mpgit:get_current_branch()
     }.

%% save data = start node + push data
save_data(Data) ->
    git_node("start"),
    fun Loop(0) -> halt(1);
        Loop(N) ->
            case catch git_node("status") of
                ok -> ok;
                _ -> Loop(N-1)
            end
    end(10),
    timer:sleep(5000),
    NodeName = get_node_name(),
    call(NodeName, git, ipush, [Data]).

%% load data = fetch data [ + stop node]
load_data() ->
    load_data([exit]).

load_data(Options) ->
    NodeName = get_node_name(),
    {ok, FetchedData} = call(NodeName, git, ifetch, []),
    case lists:member(exit, Options) of
        true ->
            git_node("stop");
        false ->
            ok
    end,
    FetchedData.

ipush(Data) ->
    case whereis(?DATAPROC) of
        undefined ->
            Self = self(),
            process_flag(trap_exit, true),
            Pid = spawn_link(fun() -> start_intermediary(Self, Data) end),
            receive
                {Pid, spawned} ->
                    ok
            after 500 ->
                    nok
            end;
        Pid when is_pid(Pid) ->
            Pid ! {self(), {push, Data}},
            receive
                Result ->
                    Result
            after 500 ->
                    nok
            end
    end.

ifetch() ->
    intermediary ! {self(), fetch},
    receive
        Result ->
            Result
    after 500 ->
            nok
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

call(NodeName, M, F, A) ->
    setup_local_node(NodeName),
    {ok, HostName} = inet:gethostname(),
    RemoteNode = list_to_atom(NodeName ++ "@" ++ HostName),
    rpc:call(RemoteNode, M, F, A).

%% Cmd = "start"|"stop"|"status"
git_node(Cmd) ->
    ErlGitScript = get_full_path(?ERLGIT_SCRIPT),
    execute([ErlGitScript, get_script_dir(), Cmd, get_node_name()]).

%% Relation: "ppid" | "pgid" | "sid"
get_os_pid(Pid, Relation) ->
    Cmd = "ps -p " ++ Pid ++ " --no-headers -o " ++ Relation,
    os_cmd(Cmd).

execute(ListOfStrings) ->
    ExeCmd = string_join(ListOfStrings),
    case os_cmd_exit(ExeCmd) of
        0 ->
            ok;
        ES ->
            ?DBG("Failed (~p) to execute: ~p~n", [ES, ExeCmd]),
            exit(failed_os_cmd_execute)
    end.

string_join(ListOfStrings) ->
    lists:flatten(lists:join(" ", ListOfStrings)).

os_cmd(Cmd) ->
    string:trim(os:cmd(Cmd)).

os_cmd_exit(Cmd) ->
    Port = open_port({spawn, Cmd}, [exit_status]),
    fun Loop() ->
            receive
                {Port, {exit_status, ES}} ->
                    ?DBG("Exit status ~p when calling: ~p~n~p~n", [ES, Cmd]),
                    ES;
                {Port, {data, Any}} ->
                    ?DBG("Data got when calling: ~p~n~p~n", [Cmd, Any]),
                    Loop();
                E ->
                    ?DBG("Sth wrong received when calling: ~p~n~p~n", [Cmd, E]),
                    Loop()
            after 5000 ->
                    ?DBG("Timeout when calling: ~p~n", [Cmd]),
                    exit(timeout)
            end
    end().

setup_local_node(RemoteNodeName) ->
    LocalNodeName = list_to_atom("escript_" ++ RemoteNodeName),
    net_kernel:start([LocalNodeName, shortnames]),
    erlang:set_cookie(node(), list_to_atom(RemoteNodeName)).

get_full_path(RelPath) ->
    filename:join(get_script_dir(), RelPath).

get_script_dir() ->
    ExePath = escript:script_name(),
    GetPathCmd = "readlink -f " ++ ExePath,
    ScriptPath = string:trim(os:cmd(GetPathCmd)),
    filename:dirname(ScriptPath).

get_node_name() ->
    ScriptPID = os:getpid(),
    SessionID = get_os_pid(ScriptPID, "sid"),
    "githooks_" ++ SessionID.

start_intermediary(Pid, Data) ->
    true = register(intermediary, self()),
    Pid ! {self(), spawned},
    intermediary_loop(Data).

intermediary_loop(Data) ->
    receive
        {Pid, {push, NewData}} ->
            Pid ! ok,
            intermediary_loop(NewData);
        {Pid, fetch} ->
            Pid ! {ok, Data},
            intermediary_loop(Data);
        _ ->
            intermediary_loop(Data)
    end.

%%==============================================================================
%% Notes
%%==============================================================================

%%------------------------------------------------------------------------------
%% PARAMTERS FOR GIT-HOOK SCRIPTS:
%%------------------------------------------------------------------------------

%% GIT-PUSH
%%   Main arguments:
%%     . RemoteAlias: for example, "origin"
%%     . RemoteUrl  : URL of remote repository
%%   Additional arguments:
%%     . LocalRef : HEAD or local branch full name of the commit to be pushed
%%     . LocalSHA : ID of the commit to be pushed
%%     . RemoteRef: remote branch full name
%%     . RemoteSHA: ID of the commit at tracking branch
%%     Examples:
%%       git push:
%%         ["refs/heads/<LBranch>","64fdf3bd357d48962931c0bb5a20a890ddf8e44f",
%%          "refs/heads/<RBranch>","9270be19400698495e064ef27d7cbd1fc1513f3e"]
%%       git push origin HEAD:<RBranch>
%%         ["HEAD","fece225d9cd14354d1a45337d1de5e690e91ea44",
%%          "refs/heads/<RBranch>","caf5cb0ee8a14fc660ba143e4f4dad95ee140f2c"]
%%       git push origin HEAD:<newBranch>
%%         ["HEAD","3f23d053c3fba458aabd8e313f7a2d1ff1285667",
%%          "refs/heads/tuan","0000000000000000000000000000000000000000"]
