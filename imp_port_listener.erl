-module(imp_port_listener).
-export([server/0, manage_conn/1]).

listener_poll_interval() -> 5000.

server() ->
    {ok, LSock} = gen_tcp:listen(6222, [binary, {packet, 0}, {active, true}]),
    listen(LSock).

listen(LSock) ->
    PollInterval = listener_poll_interval(),
    case gen_tcp:accept(LSock, PollInterval) of
        {ok, Sock} ->
            ConnMgr = spawn(imp_port_listener, manage_conn, [Sock]),
            gen_tcp:controlling_process(Sock, ConnMgr);
        {error, timeout} ->
            ok;
        {error, Err} ->
            %% Should log this or something
            io:format("Listener error ~p~n", [Err]),
            error;
        Unexpected ->
            %% Again, logging needed
            io:format("Unexpected accept result in listener: ~p~n", [Unexpected]),
            error
    end,
    receive
        stop -> ok, io:format("Listener stopped~n")
        after 0 -> listen(LSock)
    end.

manage_conn(Sock) ->
    Decoder = spawn(imp_protocol, stream_decode, [self()]),
    manage_conn(Sock, Decoder).

manage_conn(Sock, Decoder) ->
    receive
        {tcp, Sock, Data} ->
            Decoder ! {decode, Data},
            manage_conn(Sock, Decoder);
        {tcp_closed, Sock} ->
            io:format("TCP connection closed~n"),
            Decoder ! stop;
        {tcp_error, Sock, Err} ->
            %% Should log this
            io:format("TCP Error: ~p~n", [Err]),
            Decoder ! stop,
            error;
        {decode_result, {impsig, Msg}} ->
            io:format("Decoded message: ~p~n", [Msg]),
            manage_conn(Sock, Decoder);
        Unexpected ->
            io:format("Unexpected message: ~p~n", [Unexpected]),
            Decoder ! stop,
            error
    end.
