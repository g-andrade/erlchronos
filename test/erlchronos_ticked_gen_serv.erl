-module(erlchronos_ticked_gen_serv).
-behaviour(ticked_gen_server).

-export([start_link/3]).
-export([init/1,
         tick_duration/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         handle_tick/4,
         terminate/2,
         code_change/3]).

-record(state, {
          tick_counters = dict:new() :: dict:dict(term(), non_neg_integer()),
          tick_timestamps = dict:new() :: dict:dict(term(), [non_neg_integer()]),
          deadline :: pos_integer(),
          call_history = [] :: list(term()),
          cast_history = [] :: list(term()),
          info_history = [] :: list(term()),
          scheduled_tick_durations = dict:new() :: dict:dict(term(), pos_integer())
         }).

-spec start_link(Options :: [ticked_gen_server:start_option()],
                 Ticks :: [{TickId :: term(), TickDuration :: pos_integer()}],
                 TTL :: pos_integer())
        -> {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.
start_link(Options, Ticks, TTL) ->
    ticked_gen_server:start_link(?MODULE, {Ticks, TTL}, Options).

-spec init({Ticks :: [{TickId :: term(), TickDuration :: pos_integer()}], TTL :: pos_integer()}) -> {ok, #state{}}.
init({Ticks, TTL}) ->
    {ok, #state{deadline = timestamp_ms() + TTL,
                scheduled_tick_durations = dict:from_list(Ticks)}}.

-spec tick_duration(TickId :: term(), State :: #state{})
        -> {TickDuration :: non_neg_integer(), NewState :: #state{}}.
tick_duration(TickId, #state{ scheduled_tick_durations=TickDurations }=State) ->
    TickDuration = dict:fetch(TickId, TickDurations),
    {TickDuration, State}.

-spec handle_call(term(), term(), #state{}) -> {reply, term(), #state{}} | {noreply, #state{}}.
handle_call(get_counters, _From, #state{ tick_counters=Counters }=State) ->
    {reply, {ok, Counters}, State#state{ call_history=[get_counters | State#state.call_history] }};
handle_call(get_timestamps, _From, #state{ tick_timestamps=Timestamps }=State) ->
    {reply, {ok, Timestamps}, State#state{ call_history=[get_timestamps | State#state.call_history]}};
handle_call(get_histories, _From, State) ->
    #state{ call_history=CallHistory,
            cast_history=CastHistory,
            info_history=InfoHistory }=State,
    {reply, {ok, {CallHistory, CastHistory, InfoHistory}}, State};
handle_call(Request, _From, State) ->
    {noreply, State#state{ call_history=[Request | State#state.call_history] }}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State) ->
    {noreply, State#state{ cast_history=[Msg | State#state.cast_history] }}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(Info, State) ->
    {noreply, State#state{ info_history=[Info | State#state.info_history] }}.

-spec handle_tick(term(), non_neg_integer(), non_neg_integer(), #state{}) -> {noreply, #state{}}.
handle_tick(TickId, _TickGeneration, _TickDuration, #state{ deadline=Deadline }=State) ->
    NowMS = timestamp_ms(),
    case NowMS >= Deadline of
        false ->
            #state{ tick_counters=Counters,
                    tick_timestamps=Timestamps }=State,
            NewCounters = dict:update_counter(TickId, 1, Counters),
            NewTimestamps = dict:append(TickId, NowMS, Timestamps),
            {noreply, State#state{tick_counters = NewCounters,
                                  tick_timestamps = NewTimestamps}};
        true ->
            {noreply, State}
    end.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

timestamp_ms() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    ((((MegaSecs * 1000000) + Secs) * 1000000) + MicroSecs) div 1000.
