-module(life).
-behaviour(application).


%% API
-export([bang/0]).

%% Callbacks
-export([start/2, stop/1]).


-define(DIRECTIONS, ['N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW']).


%% ============================================================================
%% API
%% ============================================================================

bang() ->
    application:start(?MODULE).


%% ============================================================================
%% Callbacks
%% ============================================================================

start(_StartType, _StartArgs) ->
    {ok, X} = application:get_env(?MODULE, x),
    {ok, Y} = application:get_env(?MODULE, y),
    CellData = cell_data(X, Y),
    life_god:start_link(X, CellData).


stop(_State) ->
    ok.


%% ============================================================================
%% Internal
%% ============================================================================

cell_data(X, Y) ->
    N = X * Y,
    [cell_datum(X, N, ID) || ID <- lists:seq(1, N)].


cell_datum(X, N, ID) ->
    Name = integer_to_atom(ID),
    NeighborNames = filter_offsides(N,
        [integer_to_atom(neighbor_id(Dir, X, ID)) || Dir <- ?DIRECTIONS]
    ),
    {ID, Name, NeighborNames}.


neighbor_id(Direction, X, ID) -> ID + offset(Direction, X).


offset('N' , X) -> ensure_negative(X);
offset('NE', X) -> ensure_negative(X - 1);
offset('E' , _) ->                     1;
offset('SE', X) ->                 X + 1;
offset('S' , X) ->                 X;
offset('SW', X) ->                 X - 1;
offset('W' , _) -> ensure_negative(    1);
offset('NW', X) -> ensure_negative(X + 1).


ensure_negative(N) when N < 0 -> N;
ensure_negative(N) -> -(N).


filter_offsides(N, IDs) ->
    [ID || ID <- IDs, is_onside(N, atom_to_integer(ID))].


is_onside(_, ID) when ID < 1 -> false;
is_onside(N, ID) when ID > N -> false;
is_onside(_, _) -> true.


atom_to_integer(Atom) ->
    list_to_integer(atom_to_list(Atom)).


integer_to_atom(Integer) ->
    list_to_atom(integer_to_list(Integer)).
