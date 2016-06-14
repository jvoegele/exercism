-module(circular_buffer).
-behaviour(gen_server).

%% API functions
-export([create/1, size/1, read/1, write/2, write_attempt/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {size, buf}).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec create(Size::pos_integer()) -> pid().
create(Size) ->
    {ok, Pid} = gen_server:start_link(?MODULE, Size, []),
    Pid.

-spec size(pid()) -> pos_integer().
size(Pid) ->
    gen_server:call(Pid, size).

-spec read(pid()) -> {ok, Value::integer()}.
read(Pid) ->
    gen_server:call(Pid, read).

-spec write(pid(), Value::integer()) -> ok.
write(Pid, Value) ->
    gen_server:call(Pid, {write, Value}).

-spec write_attempt(pid(), Value::integer()) -> ok | {error, full}.
write_attempt(Pid, Value) ->
    gen_server:call(Pid, {write_attempt, Value}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Size) ->
    {ok, #state{size = Size, buf = []}}.

handle_call(size, _From, State) ->
    {reply, {ok, State#state.size}, State};

handle_call(read, _From, State = #state{buf = []}) ->
    {reply, {error, empty}, State};
handle_call(read, _From, State = #state{buf = Buf}) ->
    {Value, NewBuf} = buf_read(Buf),
    {reply, {ok, Value}, State#state{buf = NewBuf}};

handle_call({write, Value}, _From, State = #state{buf = Buf, size = Size})
  when length(Buf) >= Size ->
    {reply, ok, State#state{buf = buf_overwrite(Buf, Value)}};
handle_call({write, Value}, _From, State) ->
    {reply, ok, State#state{buf = buf_put(State#state.buf, Value)}};

handle_call({write_attempt, _Value}, _From, State = #state{buf = Buf, size = Size})
  when length(Buf) >= Size ->
    {reply, {error, full}, State};
handle_call({write_attempt, Value}, _From, State) ->
    {reply, ok, State#state{buf = buf_put(State#state.buf, Value)}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec buf_read(nonempty_list()) -> {any(), list()}.
buf_read(Buf) ->
    Value = lists:last(Buf),
    {Value, lists:droplast(Buf)}.

-spec buf_put(list(), any()) -> nonempty_list().
buf_put(Buf, Value) ->
    [Value|Buf].

-spec buf_overwrite(nonempty_list(), any()) -> nonempty_list().
buf_overwrite(Buf, Value) ->
    [Value|lists:droplast(Buf)].
