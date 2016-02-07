-module(basic_tick).
-behaviour(ticked_gen_server).

-export([start/0]).
-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         handle_tick/3,
         terminate/2,
         code_change/3]).

-define(TICK_DURATION, 50). % in milliseconds

-record(state, {
         }).

start_link() ->
    ticked_gen_server:start_link(?MODULE, [], [{ticks, [{"a simple tick", ?TICK_DURATION}]}]).

start() ->
    ticked_gen_server:start(?MODULE, [], [{ticks, [{"a simple tick", ?TICK_DURATION}]}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_tick(TickId, TickGeneration, State) ->
    io:format("tick! ~p / ~p", [TickId, TickGeneration]),
    % tick! "a simple tick" / 0
    % tick! "a simple tick" / 1
    % tick! "a simple tick" / 2
    % tick! "a simple tick" / 3
    %   ...
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
