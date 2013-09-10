-module(clock_handler).

-export([init/1,
         handle_call/2,
         handle_cast/2,
         handle_info/2,
         terminate/1]).

-record(state, {delta = 0}).

%%%===================================================================
%%% bullet_bert callbacks
%%%===================================================================

%% called when new session in browser starts
%% it's not being called e.g. between xhr long-polling requests
init([]) ->
    gen_server:cast(clock_server, {register, self()}),
    {ok, #state{}}.

%% called for every 'call' in javascript code
handle_call(inc, #state{delta=Delta}=State) ->
    NewDelta = Delta+1,
    {reply, NewDelta, State#state{delta=NewDelta}};
handle_call(dec, #state{delta=Delta}=State) ->
    NewDelta = Delta-1,
    {reply, NewDelta, State#state{delta=NewDelta}};
handle_call(_Request, State) ->
    {reply, ok, State}.

%% called for every 'cast' in javascript code
handle_cast(reset, State) ->
    {noreply, State#state{delta=0}};
handle_cast(_Request, State) ->
    {noreply, State}.

%% called when this process receives Erlang message
handle_info({time, Time}, #state{delta=Delta}=State) ->
    Seconds = calendar:datetime_to_gregorian_seconds(Time),
    Reply = {time, calendar:gregorian_seconds_to_datetime(Seconds+Delta)},
    {reply, Reply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

%% called when session in browser ends (the most probable
%% scenario is when client closed the browser and timeout occured)
terminate(_State) ->
    gen_server:cast(clock_server, {unregister, self()}),
    ok.
