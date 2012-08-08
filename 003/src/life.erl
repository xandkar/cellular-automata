-module(life).

-export([bang/1]).


-define(CHAR_DEAD,   32).  % " "
-define(CHAR_ALIVE, 111).  % "o"
-define(INTERVAL, 100).


%% ============================================================================
%% API
%% ============================================================================

bang(Args) ->
    [X, Y] = [atom_to_integer(A) || A <- Args],
    Board = init_board(X, Y),
    life_loop(Board).


%% ============================================================================
%% Internal
%% ============================================================================

life_loop(Board) ->
    ok = do_print_board(Board),
    timer:sleep(?INTERVAL),
    life_loop(next_generation(Board)).


do_print_board(Board) ->
    CharLists = array:to_list(
        array:map(
            fun(_, Row) ->
                array:to_list(
                    array:map(
                        fun(_, State) ->
                            state_to_char(State)
                        end,
                        Row
                    )
                )
            end,
            Board
        )
    ),

    ok = lists:foreach(
        fun(CharList) ->
            ok = io:format("~s~n", [CharList])
        end,
        CharLists
    ).


state_to_char(0) -> ?CHAR_DEAD;
state_to_char(1) -> ?CHAR_ALIVE.


next_generation(Board) ->
    H = array:size(Board),
    W = array:size(array:get(0, Board)),

    array:map(
        fun(Y, Row) ->
            array:map(
                fun(X, State) ->
                    Neighbors = filter_offsides(H, W, neighbors(X, Y)),
                    States = neighbor_states(Board, Neighbors),
                    LiveNeighbors = lists:sum(States),
                    new_state(State, LiveNeighbors)
                end,
                Row
            )
        end,
        Board
    ).


new_state(1, LiveNeighbors) when LiveNeighbors  <  2 -> 0;
new_state(1, LiveNeighbors) when LiveNeighbors  <  4 -> 1;
new_state(1, LiveNeighbors) when LiveNeighbors  >  3 -> 0;
new_state(0, LiveNeighbors) when LiveNeighbors =:= 3 -> 1;
new_state(State, _LiveNeighbors) -> State.


neighbor_states(Board, Neighbors) ->
    [array:get(X, array:get(Y, Board)) || {X, Y} <- Neighbors].


filter_offsides(H, W, Coordinates) ->
    [{X, Y} || {X, Y} <- Coordinates, is_onside(X, Y, H, W)].


is_onside(X, Y, H, W) when (X >= 0) and (Y >= 0) and (X < W) and (Y < H) -> true;
is_onside(_, _, _, _) -> false.


neighbors(X, Y) ->
    [{X + OffX, Y + OffY} || {OffX, OffY} <- offsets()].


offsets() ->
    [offset(D) || D <- directions()].


offset('N')  -> { 0, -1};
offset('NE') -> { 1, -1};
offset('E')  -> { 1,  0};
offset('SE') -> { 1,  1};
offset('S')  -> { 0,  1};
offset('SW') -> {-1,  1};
offset('W')  -> {-1,  0};
offset('NW') -> {-1, -1}.


directions() ->
    ['N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'].


init_board(X, Y) ->
    array:map(fun(_, _) -> init_row(X) end, array:new(Y)).


init_row(X) ->
    array:map(fun(_, _) -> init_cell_state() end, array:new(X)).


init_cell_state() ->
    crypto:rand_uniform(0, 2).


atom_to_integer(Atom) ->
    list_to_integer(atom_to_list(Atom)).
