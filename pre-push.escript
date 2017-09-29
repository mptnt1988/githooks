#!/usr/bin/env escript

main([_Remote, _Url]) ->
    case io:fread("", "~s ~s ~s ~s") of
        {ok, [LocalRef, LocalSHA, RemoteRef, RemoteSHA]} ->
            io:format("Tuan: ~p~n",
                      [[LocalRef, LocalSHA, RemoteRef, RemoteSHA]]),
            process_flag(trap_exit, true),
            Port = open_port("/dev/tty", [eof]),
            io:format("begin: ~p~n", [[{?MODULE, ?LINE}]]),
            receive
                {Port, {data, Data}} ->
                    io:format("Tuan got: ~p~n", [Data]);
                {'EXIT', Port, Reason} ->
                    io:format("Tuan exit: ~p~n", [Reason])
            end,
            io:format("end: ~p~n", [[{?MODULE, ?LINE}]]),
            ok;
        _Errors ->
            halt(1)
    end.
