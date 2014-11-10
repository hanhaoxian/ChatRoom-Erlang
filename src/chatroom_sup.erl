-module(chatroom_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([]) ->
    ChildList = [ {chatroom_ctr
                   , {chatroom_ctr, start_link, []}
                   , permanent, 2000, worker, [chatroom_ctr]
                  }
                  , {chatroom_ser
                     , {chatroom_ser, start_link, []}
                     , permanent, 2000, worker, [chatroom_ser]
                    }
                ],
    {ok, {{one_for_one, 1, 1}, ChildList}}.
