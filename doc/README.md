

# erlchronos #

Copyright (c) 2016 Guilherme Andrade

__Version:__ 2.0.0

__Authors:__ Guilherme Andrade ([`erlchronos(at)gandrade(dot)net`](mailto:erlchronos(at)gandrade(dot)net)).

`erlchronos`: Erlang/OTP gen_server wrapper with ticks

---------


### <a name="What_is_it?">What is it?</a> ###

`erlchronos` provides a [`gen_server`](http://erlang.org/doc/man/gen_server.md) wrapper, `ticked_gen_server`,
that allows one to more easily manage triggering and dealing with ticks at regular intervals by specifying two new callbacks,
`tick_duration/2` and `handle_tick/4`.

It also does away with the usual [`erlang:send_after/3`](http://www.erlang.org/doc/man/erlang.html#send_after-3),
[`timer:send_interval/2`](http://erlang.org/doc/man/timer.md#send_interval-2), etc. approaches, instead relying
on two key mechanisms for tick enforcement:
* `gen_server`'s support for specifying timeout values on callbacks - which takes care of idle-inbox periods;
* An active verification of triggering conditions on key events (inits, calls, casts and infos) - which takes care of busier periods.


### <a name="Why?">Why?</a> ###

The traditional approach of having a message sent to a `gen_server`'s inbox at regular intervals isn't always
the more appropriate, and might misbehave significantly every time the system is subjected to message bursts (among other factors), even when actively accounting for drift, due to the strictly-ordered nature of inbox consumption.

An alternative would be to change the `receive` logic itself and pattern-match against timer / higher-priority messages, but this would quickly degrade performance on flooded inboxes and create a degeneration feedback loop.

This compromise solution tries not to fiddle too much with, nor reinvent the existing building blocks.


### <a name="Pros">Pros</a> ###

* Ticks should be more precise, save for enormously flooded inboxes (whose processes never behave properly, in any case;)
* For overload scenarios, the `tick_duration/2` callback can be used to easily lower tick rate;
* Ticking logic is abstracted away.


### <a name="Cons">Cons</a> ###

* Slight execution overhead;
* Losing the ability of specify timeouts on `init` / `handle_call` / `handle_cast` / `handle_info` return values;
* Losing the ability of receiving messages (on `handle_info`) made only of the 'timeout' atom.


### <a name="How_do_I_use_it?">How do I use it?</a> ###

* Declare ticks through a new `start`/`start_link` option:

```erlang

ticked_gen_server:start(?MODULE, [], [{ticks, ["tick identifier"]}]).

```

* Implement the `ticked_gen_server` behaviour, which includes two extra callbacks:

```erlang

% TickDuration in milliseconds
-spec tick_duration(TickId :: term(), State :: term())
        -> {TickDuration :: pos_integer(), NewState :: term()}.
tick_duration("tick identifier", State) ->
    {100, State}.

-spec handle_tick(TickId :: term(), TickGeneration :: non_neg_integer(),
                  ActualTickDuration :: non_neg_integer(), State :: term())
      -> {noreply, NewState :: term()}.
handle_tick(TickId, TickGeneration, ActualTickDuration, State) ->
   % Tick!
   {noreply, State};

```

Basic example under examples/.


### <a name="Which_problems_doesn't_it_solve?">Which problems doesn't it solve?</a> ###

* When a gen_server receives events whose handling time equals to a big enough fraction of the tick duration, it risks provoking unacceptable jitter;
* When a gen_server's exection is blocked, incoming ticks risk being triggered in a burst fashion later on, but ignoring them is easy enough to do if such is a requirement.


### <a name="Future_plans">Future plans</a> ###

* Considering the possibility of having some sort of 'event classifier' callback which, through pattern matching, would allow `ticked_gen_server` to gain some insight into predicted execution times and, based on this knowledge, preemptively sacrifice execution capacity (by sleeping) and trade it for a lower likelihood of jitter;
* Wrap `gen_fsm` / `gen_event` in a similar fashion.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="erlchronos.md" class="module">erlchronos</a></td></tr>
<tr><td><a href="ticked_gen_server.md" class="module">ticked_gen_server</a></td></tr></table>

