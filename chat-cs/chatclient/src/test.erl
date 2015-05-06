%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 十一月 2014 20:48
%%%-------------------------------------------------------------------
-module(test).
-author("Administrator").

%% API
-export([start/2, test/1]).

start(BeginNum, EndNum) ->
	test([erlang:list_to_atom([$t|erlang:integer_to_list(X)]) || X <- lists:seq(BeginNum, EndNum)]).

test([]) ->
	ok;
test([Name|T]) ->
	receive
		after 200 ->
			spawn(fun() -> cr_client:new_client_auto(Name) end),
			test(T)
	end.
