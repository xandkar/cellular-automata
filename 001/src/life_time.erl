-module(life_time).
-behaviour(gen_server).


%% API
-export([start_link/3
        ,report_state/3
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
-define(CHAR_BAR,    45).  % -


-record(state, {x               :: integer()
               ,y               :: integer()
               ,cells           :: list(atom())
               ,num_cells       :: integer()
               ,state_pairs     :: list(tuple(integer(), integer())) | []
               ,replies_pending :: integer()
               ,gen_id          :: integer()
               }).


%% ============================================================================
%% API
%% ============================================================================

start_link(X, Y, Cells) ->
    ServerName = {local, ?MODULE},
    Args = [X, Y, Cells],
    Opts = [],
    gen_server:start_link(ServerName, ?MODULE, Args, Opts).


report_state(CellID, GenID, CellState) ->
    gen_server:cast(?MODULE, {report_state, {CellID, GenID, CellState}}).


%% ============================================================================
%% Callbacks
%% ============================================================================

init([X, Y, Cells]) ->
    State = #state{x               = X
                  ,y               = Y
                  ,cells           = Cells
                  ,num_cells       = length(Cells)
                  ,state_pairs     = []
                  ,replies_pending = 0
                  ,gen_id          = 0
                  },
    schedule_next_gen(),
    {ok, State}.


terminate(_Reason, State) ->
    {ok, State}.


code_change(_Old, State, _Other) ->
    {ok, State}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(next_gen,
    #state{cells=Cells
          ,num_cells=NumCells
          ,state_pairs=[]
          ,gen_id=GenID
          }=State) ->

    NewGenID = GenID + 1,
    ok = cast_all(Cells, {next_gen, NewGenID}),
    {noreply, State#state{replies_pending=NumCells, gen_id=NewGenID}};

handle_cast({report_state, {CellID, GenID, CellState}},
    #state{x=X
          ,y=Y
          ,state_pairs=StatePairs
          ,replies_pending=RepliesPending
          ,gen_id=GenID
          ,num_cells=NumCells
          }=State) ->

    NewStatePairs = [{CellID, CellState} | StatePairs],
    NewRepliesPending = RepliesPending - 1,
    NewState = State#state{replies_pending=NewRepliesPending},

    case NewRepliesPending of
        0 ->
            SortedStatePairs = lists:sort(
                fun({A, _}, {B, _}) -> A < B end,
                NewStatePairs
            ),
            StateChars = [state_to_char(S) || {_, S} <- SortedStatePairs],

            ok = io:format(
                "X: ~b Y: ~b CELLS: ~b GENERATION: ~b~n",
                [X, Y, NumCells, GenID]
            ),
            ok = do_print_bar(X),
            ok = do_print_state_chars(X, StateChars),
            ok = timer:sleep(?INTERVAL),
            schedule_next_gen(),
            {noreply, NewState#state{state_pairs=[]}};

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

schedule_next_gen() ->
    gen_server:cast(?MODULE, next_gen).


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
