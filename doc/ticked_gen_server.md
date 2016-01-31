

# Module ticked_gen_server #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

__This module defines the `ticked_gen_server` behaviour.__
<br></br>
 Required callback functions: `init/1`, `handle_call/3`, `handle_cast/2`, `handle_info/2`, `handle_tick/3`, `terminate/2`, `code_change/3`.

<a name="types"></a>

## Data Types ##




### <a name="type-debug_start_option">debug_start_option()</a> ###



<pre><code>
debug_start_option() = (trace | log | statistics | {log_to_file, FileName::string()} | {install, {Func::function(), State::term()}})
</code></pre>





### <a name="type-spawn_start_option">spawn_start_option()</a> ###



<pre><code>
spawn_start_option() = term()
</code></pre>





### <a name="type-start_option">start_option()</a> ###



<pre><code>
start_option() = ({debug, [<a href="#type-debug_start_option">debug_start_option()</a>]} | {timeout, non_neg_integer()} | {spawn_opt, [<a href="#type-spawn_start_option">spawn_start_option()</a>]} | {ticks, [<a href="#type-tick_start_option">tick_start_option()</a>]})
</code></pre>





### <a name="type-tick_start_option">tick_start_option()</a> ###



<pre><code>
tick_start_option() = {TickId::term(), TickDuration::pos_integer()}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#format_status-2">format_status/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-3">start/3</a></td><td></td></tr><tr><td valign="top"><a href="#start-4">start/4</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###


<pre><code>
code_change(OldVsn::term() | {down, term()}, State::term(), Extra::term()) -&gt; {ok, NewState::term()} | {error, Reason::term()}
</code></pre>

<br></br>



<a name="format_status-2"></a>

### format_status/2 ###


<pre><code>
format_status(Opt::normal | terminate, StatusData::[(PDict::[{Key::term(), Value::term()}]) | (State::term())]) -&gt; Status::term()
</code></pre>

<br></br>



<a name="handle_call-3"></a>

### handle_call/3 ###


<pre><code>
handle_call(Request::term(), From::{pid(), Tag::term()}, State::term()) -&gt; {reply, Reply::term(), NewState::term(), Timeout::non_neg_integer()} | {noreply, NewState::term(), Timeout::non_neg_integer()} | {stop, Reason::term(), Reply::term(), NewState::term()} | {stop, Reason::term(), NewState::term()}
</code></pre>

<br></br>



<a name="handle_cast-2"></a>

### handle_cast/2 ###


<pre><code>
handle_cast(Request::term(), State::term()) -&gt; {noreply, NewState::term(), Timeout::non_neg_integer()} | {stop, Reason::term(), NewState::term()}
</code></pre>

<br></br>



<a name="handle_info-2"></a>

### handle_info/2 ###


<pre><code>
handle_info(Info::'?GEN_SERVER_TIMEOUT_MSG' | term(), State::term()) -&gt; {noreply, NewState::term(), Timeout::non_neg_integer()} | {stop, Reason::term(), NewState::term()}
</code></pre>

<br></br>



<a name="init-1"></a>

### init/1 ###


<pre><code>
init(X1::{Mod::module(), Args::term(), TickOptions::[{ticks, <a href="#type-tick_start_option">tick_start_option()</a>}]}) -&gt; {ok, State::term(), Timeout::non_neg_integer()} | {stop, Reason::term()} | ignore
</code></pre>

<br></br>



<a name="start-3"></a>

### start/3 ###


<pre><code>
start(Mod::module(), Args::term(), Options::[<a href="#type-start_option">start_option()</a>]) -&gt; {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}
</code></pre>

<br></br>



<a name="start-4"></a>

### start/4 ###


<pre><code>
start(Name::{local, atom()} | {global, atom()} | {via, atom(), term()}, Mod::module(), Args::term(), Options::[<a href="#type-start_option">start_option()</a>]) -&gt; {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}
</code></pre>

<br></br>



<a name="start_link-3"></a>

### start_link/3 ###


<pre><code>
start_link(Mod::module(), Args::term(), Options::[<a href="#type-start_option">start_option()</a>]) -&gt; {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}
</code></pre>

<br></br>



<a name="start_link-4"></a>

### start_link/4 ###


<pre><code>
start_link(Name::{local, atom()} | {global, atom()} | {via, atom(), term()}, Mod::module(), Args::term(), Options::[<a href="#type-start_option">start_option()</a>]) -&gt; {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}
</code></pre>

<br></br>



<a name="terminate-2"></a>

### terminate/2 ###


<pre><code>
terminate(Reason::normal | shutdon | {shutdown, term()} | term, State::term()) -&gt; term()
</code></pre>

<br></br>



