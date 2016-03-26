-module(basic_tick).
-behaviour(ticked_gen_server).

-export([start/0]).
-export([start_link/0]).
-export([init/1,
         tick_duration/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         handle_tick/4,
         terminate/2,
         code_change/3]).

-define(SIMPLE_TICK_DURATION, 50). % in milliseconds

-record(state, {
         }).

start_link() ->
    ticked_gen_server:start_link(?MODULE, [], [{ticks, ["a simple tick"]}]).

start() ->
    ticked_gen_server:start(?MODULE, [], [{ticks, ["a simple tick"]}]).

start_with_custom_clock({_Module, _Function} = NanoSecondsClockSource) ->
    ticked_gen_server:start(?MODULE, [],
                            [{ticks, ["a simple tick"]},
                             {nanoseconds_clock_source, NanoSecondsClockSource}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    {ok, #state{}}.

tick_duration("a simple tick", State) ->
    {?SIMPLE_TICK_DURATION, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_tick(TickId, TickGeneration, ActualTickDuration, State) ->
    io:format("tick! ~p / ~p in ~pms", [TickId, TickGeneration, ActualTickDuration]),
    % tick! "a simple tick" / 0 in 0ms
    % tick! "a simple tick" / 1 in 50ms
    % tick! "a simple tick" / 2 in 49ms
    % tick! "a simple tick" / 3 in 50ms
    % tick! "a simple tick" / 4 in 50ms
    %   ...
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
