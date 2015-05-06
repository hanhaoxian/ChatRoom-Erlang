-module(cr_client).
-export([new_client/1, new_client_auto/1]).

-define(DEFAULT_PORT, 6612).

new_client_auto(Name) ->
    Socket = new_client(Name),
    timer(1500, fun() -> send_message(Socket, "timer message") end, 20),
    ok.

timer(Time, F, Acc) when Acc > 0->
    receive
        after Time ->
            F(),
            timer(Time, F, Acc - 1)
    end;
timer(_, _, _) ->
    void.


new_client(Name) ->
    Socket = new_socket(),
    register(Name, Pid = spawn(fun() -> loop(Socket) end)),
    gen_tcp:controlling_process(Socket, Pid),
    spawn(fun() -> loop(Socket) end),
    Socket.

new_socket()   ->
    {ok, Socket} = gen_tcp:connect("localhost", ?DEFAULT_PORT, [binary, {packet, 4}]),
    io:format("~p open~n", [Socket]),
    Socket.

loop(Socket) ->
    receive
        {tcp, Socket, Bin}  ->
            spawn(fun() -> print_message(Bin) end),
            loop(Socket);
        {send_message, Str} ->
            spawn(fun() -> send_message(Socket, Str) end),
            loop(Socket);
        {tcp_closed, Socket}    ->
            io:format("Socket closed~n");
        {stop} ->
            ok
    end.

send_message(Socket, Str) ->
    Res = gen_tcp:send(Socket, term_to_binary(Str)),
    if
        Res =:= error ->
            io:format("~p~n", [Res]);
        true ->
            void
    end.


print_message(Bin) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    case binary_to_term(Bin) of
        {_Sock, _Message} ->
            io:format("~B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B --- ~p : ~p~n", [Year, Month, Day, Hour, Minute, Second, _Sock, _Message]),
            void;
        _ ->
            void
    end.
