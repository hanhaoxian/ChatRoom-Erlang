%% @author Administrator
%% @doc @todo Add description to cr_ctr.


-module(cr_ctr).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, stop/0, add_sock/1, remove_sock/1, broadcast/2, connection_amount/0]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

add_sock(Sock) ->
    gen_server:call(?SERVER, {add, Sock}).

remove_sock(Sock) ->
    gen_server:call(?SERVER, {remove, Sock}).

broadcast(Sock, Message) ->
    gen_server:call(?SERVER, {broadcast, Sock, Message}).

connection_amount() ->
	gen_server:call(?SERVER, {broadcast, connection_amount}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {sockTab}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{sockTab = ets:new(sock, [set])}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call({add, Sock}, _From, State) ->
    Reply = ok,
	ets:insert(State#state.sockTab, {Sock, 1}),
    {reply, Reply, State};

handle_call({remove, Sock}, _From, State) ->
    Reply = ok,
	ets:delete(State#state.sockTab, Sock),
    {reply, Reply, State};

handle_call({broadcast, Sock, OldBin}, _From, State) ->
    Str = binary_to_term(OldBin),
    SockStr = io_lib:format("~p", [Sock]),%as name
	io:format("~s say: ~s~n", [SockStr, Str]),
    NewBin = term_to_binary({SockStr, Str}),

	SockList = ets:tab2list(State#state.sockTab),

    broadcast(Sock, NewBin, SockList),
    Reply = ok,
    {reply, Reply, State};

handle_call({broadcast, connection_amount}, _From, State) ->
	TabId = State#state.sockTab,
	SockList = ets:tab2list(TabId),
	Reply = {ok, length(SockList)},
	{reply, Reply, State}.

%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

broadcast(_Sock, _Bin, []) ->
    void;
broadcast(Sock, Bin, [{HSock,_}|T]) when Sock =:= HSock ->
    broadcast(Sock, Bin, T);
broadcast(Sock, Bin, [{HSock,_}|T]) ->
    _Res = spawn(fun() -> gen_tcp:send(HSock, Bin) end),
    %io:format("The current of list is ~p....~p~n", [HSock, _Res]),
    broadcast(Sock, Bin, T).
