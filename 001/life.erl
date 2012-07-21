-module(life).

-export([main/0
        ,main/1
        ]).


-define(DIRECTIONS, ['N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW']).
-define(X, 230).
-define(Y,  60).
-define(INTERVAL, 0).  % In milliseconds
-define(CHAR_DEAD,  <<" ">>).
-define(CHAR_ALIVE, <<"o">>).


main(      ) -> bang(?X, ?Y).
main([    ]) -> bang(?X, ?Y);
main([X   ]) -> bang(list_to_integer(X), list_to_integer(X));
main([X, Y]) -> bang(list_to_integer(X), list_to_integer(Y)).


%% ============================================================================
%% Life processes
%% ============================================================================

%% ----------------------------------------------------------------------------
%% Big bang
%% ----------------------------------------------------------------------------

bang(X, Y) ->
    N = X * Y,
    CellIDs = lists:seq(1, N),

    Graph =
        lists:foldl(
            fun(ID, Pairs) ->
                Neighbors = [
                    integer_to_atom(neighbor_id(D, X, ID))
                    || D <- ?DIRECTIONS
                ],
                [{integer_to_atom(ID), Neighbors} | Pairs]
            end,
            [],
            CellIDs
        ),

    Parent = self(),

    lists:foreach(
        fun({ID, Neighbors}) ->
            register(
                ID,
                spawn(fun() -> cell(ID, Parent, Neighbors) end)
            )
        end,
        [{ID, filter_offsides(N, Neighbors)} || {ID, Neighbors} <- Graph]
    ),

    CellNames = [integer_to_atom(ID) || ID <- CellIDs],

    tick(X, CellNames).


%% ----------------------------------------------------------------------------
%% Tick / tock
%% ----------------------------------------------------------------------------

tick(X, Cells) ->
    ok = send_all(Cells, {tick, self()}),
    All = Cells,
    Pending = Cells,
    StatePairs = [],
    tock(X, All, Pending, StatePairs).


tock(X, All, [], StatePairs) ->
    States =
        lists:foldl(
            fun({_ID, State}, States) -> [State | States] end,
            [],
            lists:sort(StatePairs)
        ),
    ok = do_print_states(X, States),
    ok = timer:sleep(?INTERVAL),
    tick(X, All);

tock(X, All, Pending, StatePairs) ->
    receive
        {tock, {ID, State}} ->
            NewPending = lists:delete(ID, Pending),
            NewStatePairs = [{ID, State} | StatePairs],
            tock(X, All, NewPending, NewStatePairs)
    end.


%% ----------------------------------------------------------------------------
%% Cell
%% ----------------------------------------------------------------------------

% Init
cell(MyID, MyParent, MyNeighbors) ->
    MyState = crypto:rand_uniform(0, 2),
    cell(MyID, MyParent, MyNeighbors, MyState).


cell(MyID, MyParent, MyNeighbors, MyState) ->
    receive
        {tick, MyParent} ->
            ok = send_all(MyNeighbors, {request_state, MyID}),
            cell(MyID, MyParent, MyNeighbors, MyState, {MyNeighbors, []})
    end.


% All neighbors replied
cell(MyID, MyParent, MyNeighbors, MyState, {[], States}) ->
    LiveNeighbors = lists:sum(States),
    MyNewState = new_state(MyState, LiveNeighbors),
    MyParent ! {tock, {MyID, MyNewState}},
    cell(MyID, MyParent, MyNeighbors, MyNewState);

% Awaiting requests and replies
cell(MyID, MyParent, MyNeighbors, MyState, {Pending, States}) ->
    receive
        {request_state, ID} ->
            ID ! {response_state, MyID, MyState},
            cell(MyID, MyParent, MyNeighbors, MyState, {Pending, States});

        {response_state, ID, State} ->
            NewPending = lists:delete(ID, Pending),
            NewStates = [State | States],
            cell(MyID, MyParent, MyNeighbors, MyState, {NewPending, NewStates})
    end.


%% ============================================================================
%% Rules
%% ============================================================================

new_state(1, LiveNeighbors) when LiveNeighbors  <  2 -> 0;
new_state(1, LiveNeighbors) when LiveNeighbors  <  4 -> 1;
new_state(1, LiveNeighbors) when LiveNeighbors  >  3 -> 0;
new_state(0, LiveNeighbors) when LiveNeighbors =:= 3 -> 1;
new_state(State, _LiveNeighbors) -> State.


neighbor_id(Direction, X, I) ->
    I + offset(Direction, X).


offset('N' , X) ->  -X;
offset('NE', X) -> -(X - 1);
offset('E' , _) ->       1;
offset('SE', X) ->   X + 1;
offset('S' , X) ->   X;
offset('SW', X) ->   X - 1;
offset('W' , _) ->      -1;
offset('NW', X) -> -(X + 1).


filter_offsides(N, IDs) ->
    [ID || ID <- IDs, is_onside(N, atom_to_integer(ID))].


is_onside(_, ID) when ID < 1 -> false;
is_onside(N, ID) when ID > N -> false;
is_onside(_, _) -> true.


%% ============================================================================
%% Plumbing
%% ============================================================================

atom_to_integer(Atom) ->
    list_to_integer(atom_to_list(Atom)).


integer_to_atom(Integer) ->
    list_to_atom(integer_to_list(Integer)).


send_all([], _) -> ok;
send_all([PID | PIDs], Msg) ->
    PID ! Msg,
    send_all(PIDs, Msg).


do_print_states(_, []) -> ok;
do_print_states(X, States) ->
    {XStates, RestStates} = lists:split(X, States),
    ok = io:format([state_to_char(S) || S <- XStates] ++ "\n"),
    do_print_states(X, RestStates).


state_to_char(0) -> ?CHAR_DEAD;
state_to_char(1) -> ?CHAR_ALIVE.
