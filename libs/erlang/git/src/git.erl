-module(git).

%% API exports
-export([write_info/1,
         write_info/2,
         collect_data/0,
         get_os_pid/2,
         node_start/4,
         node_status/1,
         node_stop/2,
         call/4,
         ipush/1,
         ifetch/0,
         start_intermediary/0
        ]).

%%==============================================================================
%% API functions
%%==============================================================================

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
       current_branch => get_current_branch()
     }.

%% Relation: "ppid" | "pgid" | "sid"
get_os_pid(Pid, Relation) ->
    Cmd = "ps -p " ++ Pid ++ " --no-headers -o " ++ Relation,
    os_cmd(Cmd).

node_start(ScriptPath, Name, Paths, Cmds) ->
    StrOfPaths = "\"" ++ string_join(Paths) ++ "\"",
    StrOfCmds = "\"" ++ string_join(Cmds) ++ "\"",
    Cmd = string_join([ScriptPath, Name, StrOfPaths, StrOfCmds]),
    os_cmd(Cmd).

node_stop(ScriptPath, Name) ->
    Cmd = string_join([ScriptPath, Name]),
    os_cmd(Cmd).

node_status(Name) ->
    Cmd = "erl_call -sname " ++ Name ++ " -c " ++ Name,
    Port = open_port({spawn, Cmd}, [exit_status]),
    RecvFun =
        fun Loop() ->
                receive
                    {Port, {data, _Any}} ->
                        Loop();
                    {Port, {exit_status, Status}} ->
                        Status;
                    E ->
                        io:format("Node status got sth wrong: ~p~n", [E]),
                        -1
                end
        end,
    case RecvFun() of
        0 -> started;
        1 -> stopped;
        _ -> wrong
    end.

call(NodeName, M, F, A) ->
    setup_local_node(NodeName),
    {ok, HostName} = inet:gethostname(),
    RemoteNode = list_to_atom(NodeName ++ "@" ++ HostName),
    rpc:call(RemoteNode, M, F, A).

ipush(Data) ->
    intermediary ! {self(), {push, Data}},
    receive
        Result ->
            Result
    after 500 ->
            nok
    end.

ifetch() ->
    intermediary ! {self(), fetch},
    receive
        Result ->
            Result
    after 500 ->
            nok
    end.

start_intermediary() ->
    Fun = fun() ->
                  start_intermediary([])
          end,
    spawn_link(Fun).

start_intermediary(Data) ->
    register(intermediary, self()),
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
%% Internal functions
%%==============================================================================

get_current_branch() ->
    gitrun(["symbolic-ref", "--short", "HEAD"]).

gitrun(WordList) ->
    Cmd = string_join(["git"|WordList]),
    os_cmd(Cmd).

string_join(ListOfStrings) ->
    lists:flatten(lists:join(" ", ListOfStrings)).

os_cmd(Cmd) ->
    string:trim(os:cmd(Cmd)).

setup_local_node(RemoteNodeName) ->
    LocalNodeName = list_to_atom("escript_" ++ RemoteNodeName),
    net_kernel:start([LocalNodeName, shortnames]),
    erlang:set_cookie(node(), list_to_atom(RemoteNodeName)).

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
