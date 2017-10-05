-module(git).

%% API exports
-export([write_info/1,
         write_info/2,
         collect_data/0,
         get_os_pid/2,
         start_node/4
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
    string:trim(os:cmd(Cmd)).

start_node(ScriptPath, Name, Paths, Cmds) ->
    StrOfPaths = "\"" ++ string_join(Paths) ++ "\"",
    StrOfCmds = "\"" ++ string_join(Cmds) ++ "\"",
    Cmd = string_join([ScriptPath, Name, StrOfPaths, StrOfCmds]),
    string:trim(os:cmd(Cmd)).

%%==============================================================================
%% Internal functions
%%==============================================================================

get_current_branch() ->
    run(["symbolic-ref", "--short", "HEAD"]).

run(WordList) ->
    Cmd = string_join(["git"|WordList]),
    string:trim(os:cmd(Cmd)).

string_join(ListOfStrings) ->
    lists:flatten(lists:join(" ", ListOfStrings)).

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
