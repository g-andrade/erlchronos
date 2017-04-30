% vim: set expandtab softtabstop=2 shiftwidth=4:
-module(erlchronos_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(IN_RANGE(X, Y, Range), (abs((X) - (Y)) =< Range)).

erlchronos_test_() ->
    {foreach,
        fun() ->
            %error_logger:tty(false)
            ok
        end,
        fun(_) ->
            %error_logger:tty(true)
            ok
        end,
        [
            {<<"Basic tick works">>,
                fun basic_tick_works/0
            },
            {<<"Multiple ticks work">>,
                fun multiple_ticks_work/0
            },
            {<<"A whole lot of ticks work">>,
                fun many_ticks_work/0
            },
            {<<"gen_server wrapping works">>,
                fun gen_server_wrapping_works/0
            },
            {<<"Moderate message flood is tolerable">>,
                fun moderate_message_flood_is_tolerable/0
            },
            {<<"High message flood is tolerable">>,
                fun high_message_flood_is_tolerable/0
            }
        ]
    }.

basic_tick_works() ->
    SpawnOpts = [{spawn_opts, [{priority, high}]}],
    TickDuration = 100,
    Ticks = [{basic_tick, TickDuration}],
    NapTime = 1000,
    {ok, Pid} = erlchronos_ticked_gen_serv:start_link([{ticks, proplists:get_keys(Ticks)}] ++ SpawnOpts,
                                                      Ticks, NapTime),
    timer:sleep(NapTime),
    {ok, Counters} = gen_server:call(Pid, get_counters),
    {ok, Timestamps} = gen_server:call(Pid, get_timestamps),
    exit(Pid, normal),
    BasicTickCount = dict:fetch(basic_tick, Counters),
    ?assert( ?IN_RANGE(BasicTickCount, NapTime div TickDuration, 1) ),
    TickTimestamps = dict:fetch(basic_tick, Timestamps),
    Offsets = calc_offsets(TickTimestamps),
    AvgOffset = avg(Offsets),
    Percentile90 = percentile(Offsets, 0.90),
    Percentile95 = percentile(Offsets, 0.95),
    Percentile99 = percentile(Offsets, 0.99),
    ?assert( ?IN_RANGE(AvgOffset, TickDuration, 1) ),
    ?assert( ?IN_RANGE(Percentile90, TickDuration, 1) ),
    ?assert( ?IN_RANGE(Percentile95, TickDuration, 2) ),
    ?assert( ?IN_RANGE(Percentile99, TickDuration, 5) ),
    ok.

multiple_ticks_work() ->
    SpawnOpts = [{spawn_opts, [{priority, high}]}],
    Ticks = [{tick1, 25}, {tick2, 33}, {tick3, 231}],
    NapTime = 1000,
    {ok, Pid} = erlchronos_ticked_gen_serv:start_link([{ticks, proplists:get_keys(Ticks)}] ++ SpawnOpts,
                                                      Ticks, NapTime),
    timer:sleep(NapTime),
    {ok, Counters} = gen_server:call(Pid, get_counters),
    {ok, Timestamps} = gen_server:call(Pid, get_timestamps),
    exit(Pid, normal),
    Tolerance = 1,
    ok = lists:foreach(
           fun ({TickId, TickDuration}) ->
                   %Tolerance = trunc((NapTime div Duration) * 0.1),
                   Count = dict:fetch(TickId, Counters),
                   ?assert( ?IN_RANGE(Count, NapTime div TickDuration, Tolerance) ),
                   TickTimestamps = dict:fetch(TickId, Timestamps),
                   Offsets = calc_offsets(TickTimestamps),
                   AvgOffset = avg(Offsets),
                   Percentile90 = percentile(Offsets, 0.90),
                   Percentile95 = percentile(Offsets, 0.95),
                   Percentile99 = percentile(Offsets, 0.99),
                   ?assert( ?IN_RANGE(AvgOffset, TickDuration, 1) ),
                   ?assert( ?IN_RANGE(Percentile90, TickDuration, 1) ),
                   ?assert( ?IN_RANGE(Percentile95, TickDuration, 2) ),
                   ?assert( ?IN_RANGE(Percentile99, TickDuration, 10) )
           end,
           Ticks).

many_ticks_work() ->
    SpawnOpts = [{spawn_opts, [{priority, high}]}],
    Ticks = [{{tick, N}, N} || N <- lists:seq(1, 100)],
    NapTime = 1000,
    {ok, Pid} = erlchronos_ticked_gen_serv:start_link([{ticks, proplists:get_keys(Ticks)}] ++ SpawnOpts,
                                                      Ticks, NapTime),
    timer:sleep(NapTime),
    {ok, Counters} = gen_server:call(Pid, get_counters),
    {ok, Timestamps} = gen_server:call(Pid, get_timestamps),
    exit(Pid, normal),
    Tolerance = 1,
    ok = lists:foreach(
           fun ({{tick, TickDuration}=TickId, TickDuration}) ->
                   %Tolerance = trunc((NapTime div Duration) * 0.1),
                   Count = dict:fetch(TickId, Counters),
                   ?assert( ?IN_RANGE(Count, NapTime div TickDuration, Tolerance) ),
                   TickTimestamps = dict:fetch(TickId, Timestamps),
                   Offsets = calc_offsets(TickTimestamps),
                   AvgOffset = avg(Offsets),
                   Percentile90 = percentile(Offsets, 0.90),
                   Percentile95 = percentile(Offsets, 0.95),
                   Percentile99 = percentile(Offsets, 0.99),
                   ?assert( ?IN_RANGE(AvgOffset, TickDuration, 1) ),
                   ?assert( ?IN_RANGE(Percentile90, TickDuration, 2) ),
                   ?assert( ?IN_RANGE(Percentile95, TickDuration, 3) ),
                   ?assert( ?IN_RANGE(Percentile99, TickDuration, 10) )
           end,
           Ticks).

gen_server_wrapping_works() ->
    SpawnOpts = [{spawn_opts, [{priority, high}]}],
    Ticks = [{tick, 10}],
    {ok, TickedPid} = erlchronos_ticked_gen_serv:start_link([{ticks, proplists:get_keys(Ticks)}] ++ SpawnOpts,
                                                            Ticks, 0),
    {ok, TicklessPid} = erlchronos_ticked_gen_serv:start_link(SpawnOpts, [], 0),
    ?assertMatch({ok, _}, gen_server:call(TickedPid, get_counters)),
    ?assertMatch({ok, _}, gen_server:call(TicklessPid, get_counters)),
    ?assertMatch({'EXIT',{timeout,{gen_server,call,[_|_]}}},
                 catch gen_server:call(TickedPid, get_nothing, 10)),
    ?assertMatch({'EXIT',{timeout,{gen_server,call,[_|_]}}},
                 catch gen_server:call(TicklessPid, get_nothing, 10)),
    ok = gen_server:cast(TickedPid, 'this is a cast'),
    ok = gen_server:cast(TicklessPid, 'this is a cast'),
    timer:sleep(1000),
    TickedPid ! 'this is an info',
    TicklessPid ! 'this is an info',
    {ok, {TickedPidCalls, TickedPidCasts, TickedPidInfos}} = gen_server:call(TickedPid, get_histories),
    {ok, {TicklessPidCalls, TicklessPidCasts, TicklessPidInfos}} = gen_server:call(TickedPid, get_histories),
    ok = gen_server:cast(TickedPid, stop),
    ok = gen_server:cast(TicklessPid, stop),
    ?assertEqual(TickedPidCalls, lists:reverse([get_counters, get_nothing])),
    ?assertEqual(TickedPidCalls, TicklessPidCalls),
    ?assertEqual(TickedPidCasts, ['this is a cast']),
    ?assertEqual(TickedPidCasts, TicklessPidCasts),
    ?assertEqual(TickedPidInfos, ['this is an info']),
    ?assertEqual(TickedPidInfos, TicklessPidInfos),
    ok.

moderate_message_flood_is_tolerable() ->
    SpawnOpts = [{spawn_opts, [{priority, high}]}],
    TickDuration = 10,
    Ticks = [{tick, TickDuration}],
    FloodDuration = 2000,
    {ok, Pid} = erlchronos_ticked_gen_serv:start_link([{ticks, proplists:get_keys(Ticks)}] ++ SpawnOpts,
                                                      Ticks, FloodDuration),
    ok = flood(Pid, 500, timestamp_ms() + FloodDuration),
    {ok, Counters} = gen_server:call(Pid, get_counters),
    {ok, Timestamps} = gen_server:call(Pid, get_timestamps),
    exit(Pid, normal),
    Count = dict:fetch(tick, Counters),
    Tolerance = 1,
    ?assert( ?IN_RANGE(Count, FloodDuration div TickDuration, Tolerance) ),
    TickTimestamps = dict:fetch(tick, Timestamps),
    Offsets = calc_offsets(TickTimestamps),
    AvgOffset = avg(Offsets),
    Percentile90 = percentile(Offsets, 0.90),
    Percentile95 = percentile(Offsets, 0.95),
    Percentile99 = percentile(Offsets, 0.99),
    ?assert( ?IN_RANGE(AvgOffset, TickDuration, 1) ),
    ?assert( ?IN_RANGE(Percentile90, TickDuration, 5) ),
    ?assert( ?IN_RANGE(Percentile95, TickDuration, 10) ),
    ?assert( ?IN_RANGE(Percentile99, TickDuration, 15) ),
    ok.

high_message_flood_is_tolerable() ->
    SpawnOpts = [{spawn_opts, [{priority, high}]}],
    TickDuration = 10,
    Ticks = [{tick, TickDuration}],
    FloodDuration = 4000,
    {ok, Pid} = erlchronos_ticked_gen_serv:start_link([{ticks, proplists:get_keys(Ticks)}] ++ SpawnOpts,
                                                      Ticks, FloodDuration),
    ok = flood(Pid, 5000, timestamp_ms() + FloodDuration),
    {ok, Counters} = gen_server:call(Pid, get_counters),
    {ok, Timestamps} = gen_server:call(Pid, get_timestamps),
    exit(Pid, normal),
    Count = dict:fetch(tick, Counters),
    Tolerance = 1,
    ?assert( ?IN_RANGE(Count, FloodDuration div TickDuration, Tolerance) ),
    TickTimestamps = dict:fetch(tick, Timestamps),
    Offsets = calc_offsets(TickTimestamps),
    AvgOffset = avg(Offsets),
    Percentile90 = percentile(Offsets, 0.90),
    Percentile95 = percentile(Offsets, 0.95),
    Percentile99 = percentile(Offsets, 0.99),
    ?assert( ?IN_RANGE(AvgOffset, TickDuration, 1) ),
    ?assert( ?IN_RANGE(Percentile90, TickDuration, 5) ),
    ?assert( ?IN_RANGE(Percentile95, TickDuration, 10) ),
    ?assert( ?IN_RANGE(Percentile99, TickDuration, 20) ),
    ok.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
flood(Pid, FloodLevel, Deadline) ->
    SchedulersOnline = erlang:system_info(schedulers_online),
    [spawn(fun () -> flood_exec(Pid, FloodLevel, Deadline) end)
     || _ <- lists:seq(1, SchedulersOnline - 1)],
    flood_exec(Pid, FloodLevel, Deadline).

flood_exec(Pid, FloodLevel, Deadline) ->
    case timestamp_ms() >= Deadline of
        true -> ok;
        false ->
            {message_queue_len, QueueSize} = erlang:process_info(Pid, message_queue_len),
            case QueueSize < FloodLevel of
                true  -> Pid ! "o hai ther";
                false -> ok
            end,
            flood_exec(Pid, FloodLevel, Deadline)
    end.

timestamp_ms() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    ((((MegaSecs * 1000000) + Secs) * 1000000) + MicroSecs) div 1000.

calc_offsets(L) when is_list(L), length(L) > 1 ->
    Zipped = lists:zip(lists:sublist(L, length(L) - 1),
                       lists:nthtail(1, L)),
    [B - A || {A, B} <- Zipped].

avg(L) when is_list(L), length(L) > 0 ->
    lists:sum(L) / length(L).

percentile(L, V) ->
    N = length(L),
    SortedL = lists:sort(L),
    Index = V * N,
    TruncatedIndex = trunc(Index),
    case TruncatedIndex == Index of
        true ->
            [X, Y] = lists:sublist(SortedL, TruncatedIndex, 2),
            (X + Y) / 2.0;
        false ->
            RoundedIndex = TruncatedIndex + 1,
            lists:nth(RoundedIndex, SortedL)
    end.

-endif. % ifdef(TEST)
