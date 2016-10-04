-module(bank_account).
-behaviour(gen_server).

%% API functions
-export([create/0, balance/1, deposit/2, withdraw/2, charge/2, close/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export_type([account/0]).

-record(state, {balance = 0 :: number(),
                is_open = true :: boolean()}).

-opaque account() :: pid().

%%%===================================================================
%%% API functions
%%%===================================================================

-spec create() -> account().
create() ->
    {ok, Account} = gen_server:start_link(?MODULE, 0, []),
    Account.

-spec balance(account()) -> number() | {error, account_closed}.
balance(Account) ->
    gen_server:call(Account, balance).

-spec deposit(account(), number()) -> number().
deposit(Account, Amount) ->
    gen_server:call(Account, {deposit, Amount}).

-spec withdraw(account(), number()) -> number().
withdraw(Account, Amount) ->
    gen_server:call(Account, {withdraw, Amount}).

-spec charge(account(), number()) -> number().
charge(Account, Amount) ->
    gen_server:call(Account, {charge, Amount}).

-spec close(account()) -> number().
close(Account) ->
    gen_server:call(Account, close).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(InitialBalance) ->
    {ok, #state{balance = InitialBalance}}.

handle_call(balance, _From, State) ->
    Reply = case State#state.is_open of
                true ->
                    State#state.balance;
                false ->
                    {error,account_closed}
            end,
    {reply, Reply, State};

handle_call({deposit, Amount}, _From, State) when Amount > 0 ->
    NewBalance = State#state.balance + Amount,
    {reply, Amount, State#state{balance = NewBalance}};
handle_call({deposit, _Amount}, _From, State) ->
    {reply, 0, State};

handle_call({withdraw, Amount}, _From, State = #state{balance=Balance})
  when Amount > 0 andalso Balance >= Amount ->
    NewBalance = Balance - Amount,
    {reply, Amount, State#state{balance = NewBalance}};
handle_call({withdraw, Amount}, _From, State = #state{balance=Balance})
  when Amount > 0 andalso Balance < Amount ->
    {reply, Balance, State#state{balance = 0}};
handle_call({withdraw, _Amount}, _From, State) ->
    {reply, 0, State};

handle_call({charge, Amount}, _From, State = #state{balance=Balance})
  when Amount > 0 andalso Balance >= Amount ->
    NewBalance = Balance - Amount,
    {reply, Amount, State#state{balance = NewBalance}};
handle_call({charge, _Amount}, _From, State) ->
    {reply, 0, State};

handle_call(close, _From, State) ->
    {reply, State#state.balance, State#state{is_open = false}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
