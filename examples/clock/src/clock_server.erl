-module(clock_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {clients = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    self() ! tick,
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({register, Pid}, #state{clients=Clients}=State) ->
    NewClients = [Pid | Clients],
    {noreply, State#state{clients=NewClients}};
handle_cast({unregister, Pid}, #state{clients=Clients}=State) ->
    NewClients = lists:delete(Pid, Clients),
    {noreply, State#state{clients=NewClients}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, #state{clients=Clients}=State) ->
    Time = calendar:now_to_datetime(erlang:now()),
    [Client ! {time, Time} || Client <- Clients],
    erlang:send_after(1000, self(), tick),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
