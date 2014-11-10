-module(chatroom_ctr).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, add_sock/1, remove_sock/1, broadcast/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {sockList}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
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

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{sockList = []}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

%%handle_call(_Request, _From, State) ->
%%    Reply = ok,
%%    {reply, Reply, State}.

handle_call({add, Sock}, _From, State) ->
    Reply = ok,
    SockList = [Sock|State#state.sockList],
    NewState = State#state{sockList = SockList},
    {reply, Reply, NewState};

handle_call({remove, Sock}, _From, State) ->
    Reply = ok,
    SockList = State#state.sockList -- [Sock],%%not good
    NewState = State#state{sockList = SockList},
    {reply, Reply, NewState};

handle_call({broadcast, Sock, OldBin}, _From, State) ->

    Str = binary_to_term(OldBin),
    NewStr = io_lib:format("~p ~p", [Sock, Str]),
    Bin = term_to_binary(NewStr),

    broadcast(Sock, Bin, State#state.sockList),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
broadcast(_Sock, _Bin, []) ->
    void;
broadcast(Sock, Bin, [H|T]) ->
    Res = gen_tcp:send(H, Bin),%spawn(fun() -> gen_tcp:send(H, Bin) end),%%for test
    io:format("The current of list is ~p....~p~n", [H, Res]),
    broadcast(Sock, Bin, T).
