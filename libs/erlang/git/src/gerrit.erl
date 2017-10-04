-module(gerrit).

%% API exports
-export([review/3]).

%%==============================================================================
%% API functions
%%==============================================================================

%% Config file example:
%%   #{host => "gerrit.server.com",
%%     port => "29418",
%%     project => "gerrit/project-name"}.

review(Config, Actions, CommitID)
  when is_map(Config), is_list(Actions), is_list(CommitID) ->
    {Host, Port, Project} = get_config(Config),
    ActionList = build_action_list(Actions),
    Prepend = ["-p", Port, Host, "gerrit review", "--project", Project],
    Append = [CommitID],
    run(Prepend ++ ActionList ++ Append).

%%==============================================================================
%% Internal functions
%%==============================================================================

get_config(Config) ->
    Host = maps:get(host, Config),
    Port = maps:get(port, Config, "29418"),
    Project = maps:get(project, Config),
    {Host, Port, Project}.

build_action_list(Actions) ->
    build_action_list(Actions, []).

build_action_list([{verified, NStr}|T], Acc) ->
    build_action_list(T, ["--verified", NStr | Acc]);
build_action_list([{codereview, NStr}|T], Acc) ->
    build_action_list(T, ["--code-review", NStr | Acc]);
build_action_list([submit|T], Acc) ->
    build_action_list(T, ["--submit" | Acc]);
build_action_list([{message, Str}|T], Acc) ->
    build_action_list(T, ["--message", Str | Acc]);
build_action_list([], Acc) -> Acc.

run(WordList) ->
    Cmd = lists:flatten(lists:join(" ", ["ssh"|WordList])),
    {Cmd, string:trim(os:cmd(Cmd))}.

%%==============================================================================
%% Notes
%%==============================================================================
