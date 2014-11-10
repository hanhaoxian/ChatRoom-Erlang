-module(chatroom_client1).
-export([new_test/0, nano_client_eval/1]).

-define(DEFAULT_PORT, 6612).

new_test() ->
    Socket = new_client(),
    io:format("touch!!!~p~n", [Socket]),
    spawn(fun() -> loop(Socket) end).

new_client()   ->
    {ok, Socket} = gen_tcp:connect("localhost", ?DEFAULT_PORT, [binary, {packet, 4}, {active, true}]),
    io:format("Client socket peername = ~p~n", [inet:peername(Socket)]),
    Socket.

send_message(Socket, Str) ->
    ok = gen_tcp:send(Socket, term_to_binary(Str)).


loop(Socket) ->
    receive
        {tcp, Socket, Bin}  ->
            Value = binary_to_term(Bin),
            io:format("~p~n", [Value]),
            loop(Socket)
    after 0 ->
              receive
                  {send_message, Str} ->
                      send_message(Socket, Str),
                      loop(Socket);
                  {stop} ->
                      ok
              end
    end.


nano_client_eval(Str)   ->
    {ok, Socket} = gen_tcp:connect("localhost", 6612, [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary(Str)),
    receive
        {tcp, Socket, Bin}  ->
            Value = binary_to_term(Bin),
            io:format("Client result = ~p~n", [Value]),
            gen_tcp:close(Socket)
    end.
