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

-define(CHAR_DEAD,   32).  % " "
-define(CHAR_ALIVE, 111).  % "o"
-define(CHAR_BAR,    45).  % "-"


-record(state, {x               :: integer()
               ,y               :: integer()
               ,cells           :: list(atom())
               ,num_cells       :: integer()
               ,num_dead        :: integer()
               ,num_alive       :: integer()
               ,state_pairs     :: list(tuple(integer(), integer())) | []
               ,replies_pending :: integer()
               ,gen_id          :: integer()
               ,gen_began       :: erlang:timestamp()
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
%% Callbacks (unused)
%% ============================================================================

handle_call(_Msg, _From, State)  -> {reply, ok, State}.
code_change(_Old, State, _Other) -> {ok, State}.
terminate(_Reason, State)        -> {ok, State}.


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
    ok = schedule_next_gen(),
    {ok, State}.


handle_info(next_gen,
    #state{cells=Cells
          ,num_cells=NumCells
          ,state_pairs=[]
          ,gen_id=GenID
          }=State) ->

    GenBegan = os:timestamp(),
    NewGenID = GenID + 1,
    ok = life_lib:cast_one2all(Cells, {next_gen, NewGenID}),
    NewState = State#state{replies_pending=NumCells
                          ,gen_id=NewGenID
                          ,gen_began=GenBegan
                          ,num_dead=0
                          ,num_alive=0
                          },
    {noreply, NewState};

handle_info(_Msg, State) ->
    {noreply, State}.


handle_cast({report_state, {CellID, GenID, CellState}},
    #state{x=X
          ,y=Y
          ,num_dead=NDead
          ,num_alive=NAlive
          ,state_pairs=StatePairs
          ,replies_pending=RepliesPending
          ,gen_id=GenID
          ,gen_began=GenBegan
          ,num_cells=NumCells
          }=State) ->

    NewStatePairs = [{CellID, CellState} | StatePairs],
    NewRepliesPending = RepliesPending - 1,
    {NewNDead, NewNAlive} = increment_dead_or_alive(CellState, NDead, NAlive),
    NewState = State#state{replies_pending=NewRepliesPending
                          ,num_dead=NewNDead
                          ,num_alive=NewNAlive
                          },

    case NewRepliesPending of
        0 ->
            SortedStatePairs = lists:sort(
                fun({A, _}, {B, _}) -> A < B end,
                NewStatePairs
            ),
            StateChars = [state_to_char(S) || {_, S} <- SortedStatePairs],

            GenDuration = timer:now_diff(os:timestamp(), GenBegan) / 1000000,

            ok = life_observer:log_generation(GenID, GenDuration, NewNDead, NewNAlive),

            ok = io:format(
                "X: ~b Y: ~b CELLS: ~b DEAD: ~b ALIVE: ~b GENERATION: ~b DURATION: ~f~n",
                [X, Y, NumCells, NewNDead, NewNAlive, GenID, GenDuration]
            ),
            ok = do_print_bar(X),
            ok = do_print_state_chars(X, StateChars),

            ok = schedule_next_gen(),
            {noreply, NewState#state{state_pairs=[]}};

        _N ->
            {noreply, NewState#state{state_pairs=NewStatePairs}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.


%% ============================================================================
%% Internal
%% ============================================================================

increment_dead_or_alive(0, NDead, NAlive) -> {NDead + 1, NAlive};
increment_dead_or_alive(1, NDead, NAlive) -> {NDead, NAlive + 1}.


schedule_next_gen() ->
    erlang:send_after(?INTERVAL, self(), next_gen),
    ok.


state_to_char(0) -> ?CHAR_DEAD;
state_to_char(1) -> ?CHAR_ALIVE.


do_print_state_chars(_, []) -> ok;
do_print_state_chars(X, Chars) ->
    {XChars, RestChars} = lists:split(X, Chars),
    ok = io:format([XChars, $\n]),
    do_print_state_chars(X, RestChars).


do_print_bar(X) ->
    io:format("~s~n", [[?CHAR_BAR || _ <- lists:seq(1, X - 1)]]).
