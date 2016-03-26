-module(erlchronos).
-export([now_timestamp_ns/0]). -ignore_xref({now_timestamp_ns, 0}).

-type timestamp() :: integer().
-export_type([timestamp/0]).

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
