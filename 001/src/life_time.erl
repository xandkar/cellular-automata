-module(life_time).
-behaviour(gen_server).


%% API
-export([start_link/2
        ,tock/2
        ]).

%% Callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).


-define(INTERVAL, 100).  % In milliseconds

-define(CHAR_DEAD,   32).  % Space
-define(CHAR_ALIVE, 111).  % o
-define(CHAR_BAR,    61).  % =


-record(state, {x               :: integer()
               ,cells           :: list(atom())
               ,num_cells       :: integer()
               ,state_pairs     :: list(tuple(integer(), integer())) | []
               ,replies_pending :: integer()
               ,gen_count = 0   :: integer()
               }).


%% ============================================================================
%% API
%% ============================================================================

start_link(X, Cells) ->
    ServerName = {local, ?MODULE},
    Args = [X, Cells],
    Opts = [],
    gen_server:start_link(ServerName, ?MODULE, Args, Opts).


tock(CellID, CellState) ->
    gen_server:cast(?MODULE, {tock, {CellID, CellState}}).


%% ============================================================================
%% Callbacks
%% ============================================================================

init([X, Cells]) ->
    State = #state{x=X
                  ,cells=Cells
                  ,num_cells=length(Cells)
                  ,state_pairs=[]
                  ,replies_pending=0
                  },
    schedule_next_tick(),
    {ok, State}.


terminate(_Reason, State) ->
    {ok, State}.


code_change(_Old, State, _Other) ->
    {ok, State}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(next_tick,
    #state{cells=Cells
          ,num_cells=NumCells
          ,state_pairs=[]
          }=State) ->

    ok = cast_all(Cells, tick),
    {noreply, State#state{replies_pending=NumCells}};

handle_cast({tock, {ID, CellState}},
    #state{x=X
          ,state_pairs=StatePairs
          ,replies_pending=RepliesPending
          ,gen_count=GenCount
          ,num_cells=NumCells
          }=State) ->

    NewStatePairs = [{ID, CellState} | StatePairs],
    NewRepliesPending = RepliesPending - 1,
    NewState = State#state{replies_pending=NewRepliesPending},

    case NewRepliesPending of
        0 ->
            NewGenCount = GenCount + 1,
            SortedStatePairs = lists:sort(NewStatePairs),
            StateChars = [state_to_char(S) || {_, S} <- SortedStatePairs],
            ok = do_print_bar(X),
            ok = io:format(
                "CELLS: ~b GENERATIONS: ~b~n",
                [NumCells, NewGenCount]
            ),
            ok = do_print_bar(X),
            ok = do_print_state_chars(X, StateChars),
            ok = do_print_bar(X),
            ok = timer:sleep(?INTERVAL),
            schedule_next_tick(),
            {noreply, NewState#state{state_pairs=[], gen_count=NewGenCount}};

        _N ->
            {noreply, NewState#state{state_pairs=NewStatePairs}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Msg, State) ->
    {noreply, State}.


%% ============================================================================
%% Internal
%% ============================================================================

schedule_next_tick() ->
    gen_server:cast(?MODULE, next_tick).


cast_all([], _) -> ok;
cast_all([Server | Servers], Msg) ->
    ok = gen_server:cast(Server, Msg),
    cast_all(Servers, Msg).


state_to_char(0) -> ?CHAR_DEAD;
state_to_char(1) -> ?CHAR_ALIVE.


do_print_state_chars(_, []) -> ok;
do_print_state_chars(X, Chars) ->
    {XChars, RestChars} = lists:split(X, Chars),
    ok = io:format([XChars, $\n]),
    do_print_state_chars(X, RestChars).


do_print_bar(X) ->
    io:format("~s~n", [[?CHAR_BAR || _ <- lists:seq(1, X - 1)]]).
