-module(ticked_gen_server).
-behaviour(gen_server).
-author('Guilherme Andrade <erlchronos(at)gandrade(dot)net>').

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
%% Module Metadata Function Exports
%% ------------------------------------------------------------------

-ignore_xref([{behaviour_info, 1}]).

%% ------------------------------------------------------------------
%% ticked_gen_server Behaviour Callbacks
%% ------------------------------------------------------------------

%% Like gen_server's <a href="http://erlang.org/doc/man/gen_server.html#Module:init-1">Module:init/1</a>
%% callback, excluding support for timeout values on return.
-callback init(Args :: term()) ->
    {ok, State :: term()} |
    {stop, Reason :: term()} | ignore.

%% Get current tick duration for next scheduling
-callback tick_duration(TickId :: term(), State :: term()) -> {TickDuration :: pos_integer(), NewState :: term()}.

%% Like gen_server's <a href="http://erlang.org/doc/man/gen_server.html#Module:handle_call-3">Module:handle_call/3</a>
%% callback, excluding support for timeout values on return.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {noreply, NewState :: term()} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.

%% Like gen_server's <a href="http://erlang.org/doc/man/gen_server.html#Module:handle_cast-2">Module:handle_cast/2</a>
%% callback, excluding support for timeout values on return.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.

%% Like gen_server's <a href="http://erlang.org/doc/man/gen_server.html#Module:handle_info-2">Module:handle_info/2</a>
%% callback, excluding support for timeout values on return.
-callback handle_info(Info :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.

%% Handle a tick
-callback handle_tick(TickId :: term(), TickGeneration :: non_neg_integer(),
                      ActualTickDuration :: non_neg_integer(), State :: term()) ->
    {noreply, NewState :: term()}.

%% Like gen_server's <a href="http://erlang.org/doc/man/gen_server.html#Module:terminate-2">Module:terminate/2</a>
%% callback.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().

%% Like gen_server's <a href="http://erlang.org/doc/man/gen_server.html#Module:code_change-3">Module:code_change/3</a>
%% callback.
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.

%% Like gen_server's <a href="http://erlang.org/doc/man/gen_server.html#Module:format_status-2">Module:format_status/2</a>
%% callback.
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
-define(PROCDIC_CLOCK_SOURCE, '$ticked_gen_server.clock_source').
-define(GEN_SERVER_TIMEOUT_MSG, timeout).
-define(DEFAULT_CLOCK_SOURCE, {erlchronos, now_timestamp_ns}).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(tick, {
          id :: term(),
          deadline_ns :: erlchronos:timestamp(),
          generation :: non_neg_integer(),
          prev_now_ns :: erlchronos:timestamp()
}).
-type tick() :: #tick{}.

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type start_option() :: ({debug, [debug_start_option()]} |
                         {timeout, non_neg_integer()} |
                         {spawn_opt, [spawn_start_option()]} |
                         {ticks, [tick_id()]} |
                         {nanoseconds_clock_source, nanoseconds_clock_source()}).
-export_type([start_option/0]).

-type debug_start_option() :: (trace | log | statistics | {log_to_file, FileName :: string()} |
                               {install, {Func :: fun(), State :: term()}}).
-export_type([debug_start_option/0]).

-type spawn_start_option() :: term().
-export_type([spawn_start_option/0]).

-type tick_id() :: term().
-export_type([tick_id/0]).

%% 0-arity function that returns an integer nanoseconds timestamp
-type nanoseconds_clock_source() :: {Module :: module(), Function :: atom()}.
-export_type([nanoseconds_clock_source/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start(Mod :: module(), Args :: term(), Options :: [start_option()])
        -> {ok, pid()} |
           {error, {already_started, pid()}} |
           {error, Reason :: term()}.
start(Mod, Args, Options) ->
    {TickOptions, GenServerOptions} = split_start_options(Options),
    gen_server:start(?MODULE, {Mod, Args, TickOptions}, GenServerOptions).

-spec start(Name :: {local, atom()} | {global, atom()} | {via, atom(), term()},
            Mod :: module(), Args :: term(), Options :: [start_option()])
        -> {ok, pid()} |
           {error, {already_started, pid()}} |
           {error, Reason :: term()}.
start(Name, Mod, Args, Options) ->
    {TickOptions, GenServerOptions} = split_start_options(Options),
    gen_server:start(Name, ?MODULE, {Mod, Args, TickOptions}, GenServerOptions).

-spec start_link(Mod :: module(), Args :: term(), Options :: [start_option()])
        -> {ok, pid()} |
           {error, {already_started, pid()}} |
           {error, Reason :: term()}.
start_link(Mod, Args, Options) ->
    {TickOptions, GenServerOptions} = split_start_options(Options),
    gen_server:start_link(?MODULE, {Mod, Args, TickOptions}, GenServerOptions).

-spec start_link(Name :: {local, atom()} | {global, atom()} | {via, atom(), term()},
                 Mod :: module(), Args :: term(), Options :: [start_option()])
        -> {ok, pid()} |
           {error, {already_started, pid()}} |
           {error, Reason :: term()}.
start_link(Name, Mod, Args, Options) ->
    {TickOptions, GenServerOptions} = split_start_options(Options),
    gen_server:start_link(Name, ?MODULE, {Mod, Args, TickOptions}, GenServerOptions).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init({Mod :: module(), Args :: term(),
            TickOptions :: [{ticks, [tick_id()]} | {nanoseconds_clock_source, nanoseconds_clock_source()}]})
        -> {ok, State :: term(), Timeout :: non_neg_integer()} |
           {stop, Reason :: term()} | ignore.
init({Mod, Args, TickOptions}) ->
    TickIds = proplists:get_value(ticks, TickOptions, []),
    ClockSource = proplists:get_value(nanoseconds_clock_source, TickOptions, ?DEFAULT_CLOCK_SOURCE),
    NowNS = now_timestamp_ns(ClockSource),
    Ticks = lists:map(
              fun (TickId) ->
                      #tick{id = TickId,
                            deadline_ns = NowNS,
                            generation = 0,
                            prev_now_ns = NowNS}
              end,
              TickIds),
    undefined = put(?PROCDIC_MODULE, Mod),
    undefined = put(?PROCDIC_TICKS, Ticks),
    undefined = put(?PROCDIC_CLOCK_SOURCE, ClockSource),
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


-spec handle_tick_deadlines(State :: term(), UntriggeredTicks :: [tick()], NowNS :: erlchronos:timestamp(),
                            Mod :: module(), TriggeredTicks :: [tick()])
        -> {NewState :: term(), NewTicks :: [tick()]}.
handle_tick_deadlines(State, [#tick{ deadline_ns=DeadlineNS }=Tick | UntriggeredTicks],
                      NowNS, Mod, TriggeredTicks)
  when DeadlineNS =< NowNS ->
    #tick{id = Id,
          generation = Generation,
          prev_now_ns = PrevNowNS} = Tick,
    ActualDurationNS = NowNS - PrevNowNS,
    ActualDurationMS = ActualDurationNS div 1000000,
    {noreply, NewState} = Mod:handle_tick(Id, Generation, ActualDurationMS, State),

    {NewDurationMS, NewState2} = Mod:tick_duration(Id, NewState),
    NewDurationNS = NewDurationMS * 1000000,
    NewTick = Tick#tick{deadline_ns = DeadlineNS + NewDurationNS,
                        generation = Generation + 1,
                        prev_now_ns = NowNS},
    handle_tick_deadlines(NewState2, UntriggeredTicks, NowNS, Mod, [NewTick | TriggeredTicks]);
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

-spec now_timestamp_ns() -> erlchronos:timestamp().
now_timestamp_ns() ->
  now_timestamp_ns(get(?PROCDIC_CLOCK_SOURCE)).

-spec now_timestamp_ns(nanoseconds_clock_source()) -> erlchronos:timestamp().
now_timestamp_ns({Module, Function}) ->
  Module:Function().

-spec split_start_options([start_option()]) -> {[{ticks, [tick_id()]} |
                                                 {nanoseconds_clock_source, nanoseconds_clock_source()}],
                                                [start_option()]}.
split_start_options(StartOptions) ->
    lists:partition(fun ({ticks, _Ticks}) -> true;
                        ({nanoseconds_clock_source, _MF}) -> true;
                        (_) -> false
                    end,
                    StartOptions).
