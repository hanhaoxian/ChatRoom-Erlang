-module(chatroom_client).
-export([new_client/0, send_message/2, new_client_spawn_loop/0]).

-define(DEFAULT_PORT, 6612).

new_client()   ->
    {ok, Socket} = gen_tcp:connect("localhost", ?DEFAULT_PORT, [binary, {packet, 4}]),
    loop(Socket),
    Socket.

new_client_spawn_loop()   ->
    {ok, Socket} = gen_tcp:connect("localhost", ?DEFAULT_PORT, [binary, {packet, 4}, {active, once}]),
    gen_tcp:controlling_process(Socket, spawn(fun() -> loop(Socket) end)),
    Socket.


send_message(Socket, Str) ->
    ok = gen_tcp:send(Socket, term_to_binary(Str)).


loop(Socket) ->
    inet:setopts(Socket,[{active,once}]),
    receive
        {tcp, Socket, Bin}  ->
            Value = binary_to_term(Bin),
            io:format("~p~n", [Value]),
            loop(Socket);
        {stop} ->
            void
    end.
