-module(ticked_gen_server).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/3]).      -ignore_xref([{start, 3}]).
-export([start/4]).      -ignore_xref([{start, 4}]).
-export([start_link/3]). -ignore_xref([{start_link, 3}]).
-export([start_link/4]). -ignore_xref([{start_link, 4}]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1]).          -ignore_xref([{init, 1}]).
-export([handle_call/3]).   -ignore_xref([{handle_call, 3}]).
-export([handle_cast/2]).   -ignore_xref([{handle_cast, 2}]).
-export([handle_info/2]).   -ignore_xref([{handle_info, 2}]).
-export([code_change/3]).   -ignore_xref([{code_change, 3}]).
-export([terminate/2]).     -ignore_xref([{terminate, 2}]).
-export([format_status/2]). -ignore_xref([{format_status, 2}]).

%% ------------------------------------------------------------------
%% ticked_gen_server Behaviour Callbacks
%% ------------------------------------------------------------------

-callback init(Args :: term()) ->
    {ok, State :: term()} |
    {stop, Reason :: term()} | ignore.

-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {noreply, NewState :: term()} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.

-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.

-callback handle_info(Info :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.

-callback handle_tick(TickId :: term(), TickGeneration :: non_neg_integer(),
                      State :: term()) ->
    {noreply, NewState :: term()}.

-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().

-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.

-ifndef(pre18).
% Explicitly optional callbacks weren't supported before release 18
-callback format_status(Opt, StatusData) -> Status when
      Opt :: 'normal' | 'terminate',
      StatusData :: [PDict | State],
      PDict :: [{Key :: term(), Value :: term()}],
      State :: term(),
      Status :: term().

-optional_callbacks([format_status/2]).
-endif.

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(PROCDIC_MODULE, '$ticked_gen_server.module').
-define(PROCDIC_TICKS, '$ticked_gen_server.ticks').
-define(GEN_SERVER_TIMEOUT_MSG, timeout).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(tick, {
          id :: term(),
          duration_ns :: pos_integer(),
          deadline_ns :: timestamp(),
          generation :: non_neg_integer(),
          skip_missed :: boolean()
}).
-type tick() :: #tick{}.

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type start_option() :: ({debug, [debug_start_option()]} |
                         {timeout, non_neg_integer()} |
                         {spawn_opt, [spawn_start_option()]} |
                         {ticks, [tick_start_option()]}).
-export_type([start_option/0]).

-type debug_start_option() :: (trace | log | statistics | {log_to_file, FileName :: string()} |
                               {install, {Func :: fun(), State :: term()}}).
-export_type([debug_start_option/0]).

-type spawn_start_option() :: term().
-export_type([spawn_start_option/0]).

-type tick_start_option() ::
    ({TickId :: term(), TickDuration :: pos_integer()} |
     {TickId :: term(), TickDuration :: pos_integer(), TickSettings :: [tick_setting()]}).
-export_type([tick_start_option/0]).

-type tick_setting() :: skip_missed | dont_skip_missed.
-export_type([tick_setting/0]).

-ifdef(pre18).
-type timestamp() :: non_neg_integer().
-else.
-type timestamp() :: integer().
-endif.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start(Mod :: module(), Args :: term(), Options :: [start_option()])
        -> {ok, pid()} |
           {error, {already_started, pid()}} |
           {error, Reason :: term()}.
start(Mod, Args, Options) ->
    {TickedGServerOptions, GenServerOptions} = split_start_options(Options),
    gen_server:start(?MODULE, {Mod, Args, TickedGServerOptions}, GenServerOptions).

-spec start(Name :: {local, atom()} | {global, atom()} | {via, atom(), term()},
            Mod :: module(), Args :: term(), Options :: [start_option()])
        -> {ok, pid()} |
           {error, {already_started, pid()}} |
           {error, Reason :: term()}.
start(Name, Mod, Args, Options) ->
    {TickedGServerOptions, GenServerOptions} = split_start_options(Options),
    gen_server:start(Name, ?MODULE, {Mod, Args, TickedGServerOptions}, GenServerOptions).

-spec start_link(Mod :: module(), Args :: term(), Options :: [start_option()])
        -> {ok, pid()} |
           {error, {already_started, pid()}} |
           {error, Reason :: term()}.
start_link(Mod, Args, Options) ->
    {TickedGServerOptions, GenServerOptions} = split_start_options(Options),
    gen_server:start_link(?MODULE, {Mod, Args, TickedGServerOptions}, GenServerOptions).

-spec start_link(Name :: {local, atom()} | {global, atom()} | {via, atom(), term()},
                 Mod :: module(), Args :: term(), Options :: [start_option()])
        -> {ok, pid()} |
           {error, {already_started, pid()}} |
           {error, Reason :: term()}.
start_link(Name, Mod, Args, Options) ->
    {TickedGServerOptions, GenServerOptions} = split_start_options(Options),
    gen_server:start_link(Name, ?MODULE, {Mod, Args, TickedGServerOptions}, GenServerOptions).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init({Mod :: module(), Args :: term(), TickedGServerOptions :: [{ticks, tick_start_option()}]})
        -> {ok, State :: term(), Timeout :: non_neg_integer()} |
           {stop, Reason :: term()} | ignore.
init({Mod, Args, TickedGServerOptions}) ->
    RawTickOptions = proplists:get_value(ticks, TickedGServerOptions, []),
    TickOptions = lists:map(
                    fun ({TickId, TickDurationMS}) ->
                        {TickId, TickDurationMS, []};
                        ({TickId, TickDurationMS, TickSettings}) ->
                        {TickId, TickDurationMS, TickSettings}
                    end,
                    RawTickOptions),
    Ticks = lists:map(
              fun ({TickId, TickDurationMS, TickSettings}) ->
                  DontSkipMissed = (lists:member(dont_skip_missed, TickSettings) orelse
                                    not lists:member(skip_missed, TickSettings)),
                  #tick{id = TickId,
                        duration_ns = TickDurationMS * 1000000,
                        deadline_ns = now_timestamp_ns(),
                        generation = 0,
                        skip_missed = not DontSkipMissed}
              end,
              TickOptions),
    undefined = put(?PROCDIC_MODULE, Mod),
    undefined = put(?PROCDIC_TICKS, Ticks),
    inject_init_timeout(Mod:init(Args)).

-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term())
        -> {reply, Reply :: term(), NewState :: term(), Timeout :: non_neg_integer()} |
           {noreply, NewState :: term(), Timeout :: non_neg_integer()} |
           {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
           {stop, Reason :: term(), NewState :: term()}.
handle_call(Request, From, State) ->
    Mod = get(?PROCDIC_MODULE),
    inject_handle_call_timeout(Mod:handle_call(Request, From, State)).

-spec handle_cast(Request :: term(), State :: term())
        -> {noreply, NewState :: term(), Timeout :: non_neg_integer()} |
           {stop, Reason :: term(), NewState :: term()}.
handle_cast(Request, State) ->
    Mod = get(?PROCDIC_MODULE),
    inject_handle_cast_timeout(Mod:handle_cast(Request, State)).

-spec handle_info(Info :: ?GEN_SERVER_TIMEOUT_MSG | term(), State :: term())
        -> {noreply, NewState :: term(), Timeout :: non_neg_integer()} |
           {stop, Reason :: term(), NewState :: term()}.
handle_info(?GEN_SERVER_TIMEOUT_MSG, State) ->
    inject_handle_info_timeout(handle_timeouts(State));
handle_info(Info, State) ->
    Mod = get(?PROCDIC_MODULE),
    inject_handle_info_timeout(Mod:handle_info(Info, State)).

-spec code_change(OldVsn :: term() | {down, term()}, State :: term(), Extra :: term())
        -> {ok, NewState :: term()} | {error, Reason :: term()}.
code_change(OldVsn, State, Extra) ->
    Mod = get(?PROCDIC_MODULE),
    Mod:code_change(OldVsn, State, Extra).

-spec terminate(Reason :: normal | shutdon | {shutdown, term()} | term, State :: term())
        -> term().
terminate(Reason, State) ->
    Mod = get(?PROCDIC_MODULE),
    Mod:terminate(Reason, State).

-spec format_status(Opt :: normal | terminate,
                    StatusData :: [(PDict :: [{Key :: term(), Value :: term()}])
                                   | (State :: term())]) -> Status :: term().
format_status(Opt, StatusData) ->
    Mod = get(?PROCDIC_MODULE),
    % Optional callback; if the callback module doesn't implement it, we'll
    % simply throw back to gen_server and it will fallback to default formatting.
    Mod:format_status(Opt, StatusData).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec handle_timeouts(State :: term()) -> {noreply, NewState :: term()}.
handle_timeouts(State) ->
    NowNS = now_timestamp_ns(),
    Mod = get(?PROCDIC_MODULE),
    Ticks = get(?PROCDIC_TICKS),
    SortedTicks = lists:keysort(#tick.deadline_ns, Ticks),
    {NewState, NewTicks} = handle_tick_deadlines(State, SortedTicks, NowNS, Mod, []),
    _ = put(?PROCDIC_TICKS, NewTicks),
    {noreply, NewState}.


-spec handle_tick_deadlines(State :: term(), UntriggeredTicks :: [tick()], NowNS :: timestamp(),
                            Mod :: module(), TriggeredTicks :: [tick()])
        -> {NewState :: term(), NewTicks :: [tick()]}.
handle_tick_deadlines(State, [#tick{ deadline_ns=DeadlineNS }=Tick | UntriggeredTicks],
                      NowNS, Mod, TriggeredTicks)
  when DeadlineNS =< NowNS ->
    #tick{id = Id,
          duration_ns = DurationNS,
          generation = Generation,
          skip_missed = SkipMissed} = Tick,
    GenerationsSkip = case SkipMissed of
                        false -> 0;
                        true  -> trunc((NowNS - DeadlineNS) / DurationNS)
                      end,
    {noreply, NewState} = Mod:handle_tick(Id, Generation + GenerationsSkip, State),
    NewTick = Tick#tick{deadline_ns = DeadlineNS + ((1 + GenerationsSkip) * DurationNS),
                        generation = Generation + GenerationsSkip + 1},
    handle_tick_deadlines(NewState, UntriggeredTicks, NowNS, Mod, [NewTick | TriggeredTicks]);
handle_tick_deadlines(State, UntriggeredTicks, _NowNS, _Mod, TriggeredTicks) ->
    {State, TriggeredTicks ++ UntriggeredTicks}.

-spec inject_init_timeout({ok, State :: term()} |
                          {stop, Reason :: term()} |
                          ignore)
        -> {ok, NewState :: term(), Timeout :: non_neg_integer()} |
           {ok, NewState :: term()} |
           {stop, Reason :: term()} |
           ignore.
inject_init_timeout({ok, State}=Ret) ->
    {NewState, TimeoutValue} = inject_and_maybe_trigger_timeouts(State),
    case TimeoutValue of
        none -> Ret;
        _    -> {ok, NewState, TimeoutValue}
    end;
inject_init_timeout({stop, _Reason}=Ret) ->
    Ret;
inject_init_timeout(ignore=Ret) ->
    Ret.

-spec inject_handle_call_timeout({reply, Reply :: term(), NewState :: term()} |
                                 {noreply, NewState :: term()} |
                                 {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                                 {stop, Reason :: term(), NewState :: term()})
        -> {reply, Reply :: term(), NewerState :: term(), Timeout :: non_neg_integer()} |
           {reply, Reply :: term(), NewerState :: term()} |
           {noreply, NewerState :: term(), Timeout :: non_neg_integer()} |
           {noreply, NewerState :: term()} |
           {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
           {stop, Reason :: term(), NewState :: term()}.
inject_handle_call_timeout({reply, Reply, NewState}=Ret) ->
    {NewerState, TimeoutValue} = inject_and_maybe_trigger_timeouts(NewState),
    case TimeoutValue of
        none -> Ret;
        _    -> {reply, Reply, NewerState, TimeoutValue}
    end;
inject_handle_call_timeout({noreply, NewState}=Ret) ->
    {NewerState, TimeoutValue} = inject_and_maybe_trigger_timeouts(NewState),
    case TimeoutValue of
        none -> Ret;
        _    -> {noreply, NewerState, TimeoutValue}
    end;
inject_handle_call_timeout({stop, _Reason, _Reply, _NewState}=Ret) ->
    Ret;
inject_handle_call_timeout({stop, _Reason, _NewState}=Ret) ->
    Ret.

-spec inject_handle_cast_timeout({noreply, NewState :: term()} |
                                 {stop, Reason :: term(), NewState :: term()})
        -> {noreply, NewerState :: term(), Timeout :: non_neg_integer()} |
           {noreply, NewerState :: term()} |
           {stop, Reason :: term(), NewState :: term()}.
inject_handle_cast_timeout({noreply, NewState}=Ret) ->
    {NewerState, TimeoutValue} = inject_and_maybe_trigger_timeouts(NewState),
    case TimeoutValue of
        none -> Ret;
        _    -> {noreply, NewerState, TimeoutValue}
    end;
inject_handle_cast_timeout({stop, _Reason, _NewState}=Ret) ->
    Ret.

-spec inject_handle_info_timeout({noreply, NewState :: term()} |
                                 {stop, Reason :: term(), NewState :: term()})
        -> {noreply, NewerState :: term(), Timeout :: non_neg_integer()} |
           {noreply, NewerState :: term()} |
           {stop, Reason :: term(), NewState :: term()}.
inject_handle_info_timeout({noreply, NewState}=Ret) ->
    {NewerState, TimeoutValue} = inject_and_maybe_trigger_timeouts(NewState),
    case TimeoutValue of
        none -> Ret;
        _    -> {noreply, NewerState, TimeoutValue}
    end;
inject_handle_info_timeout({stop, _Reason, _NewState}=Ret) ->
    Ret.

-spec inject_and_maybe_trigger_timeouts(State :: term())
        -> {NewState :: term(), Timeout :: non_neg_integer() | none}.
inject_and_maybe_trigger_timeouts(State) ->
    MinTimeoutValue = min_timeout_value_ms(),
    case MinTimeoutValue of
        0 ->
            {noreply, NewState} = handle_timeouts(State),
            {NewState, min_timeout_value_ms()};
        _ ->
            {State, MinTimeoutValue}
    end.

-spec min_timeout_value_ms() -> non_neg_integer() | none.
min_timeout_value_ms() ->
    NowNS = now_timestamp_ns(),
    case get(?PROCDIC_TICKS) of
        [] -> none;
        Ticks ->
            [#tick{ deadline_ns=MinDeadlineNS } | _]
                = lists:keysort(#tick.deadline_ns, Ticks),
            round(max(0, MinDeadlineNS - NowNS) / 1000000)
    end.

-ifdef(pre18).
-spec now_timestamp_ns() -> timestamp().
now_timestamp_ns() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    ((((MegaSecs * 1000000) + Secs) * 1000000) + MicroSecs) * 1000.
-else.
-spec now_timestamp_ns() -> timestamp().
now_timestamp_ns() ->
    erlang:monotonic_time(nano_seconds).
-endif.

-spec split_start_options([start_option()]) -> {[{ticks, tick_start_option()}],
                                                [start_option()]}.
split_start_options(StartOptions) ->
    lists:partition(fun ({ticks, _Ticks}) -> true;
                        (_) -> false
                    end,
                    StartOptions).
