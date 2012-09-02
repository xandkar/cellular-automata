-module(life_god).
-behaviour(supervisor).


%% API
-export([start_link/3]).

%% Callbacks
-export([init/1]).


%% Helper macro for declaring children of supervisor
-define(CHILD(Type, I, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).


%% ============================================================================
%% API
%% ============================================================================

start_link(X, Y, CellData) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [X, Y, CellData]).


%% ============================================================================
%% Callbacks
%% ============================================================================

init([X, Y, CellData]) ->
    CellNames = [Name || {_, Name, _} <- CellData],
    RestartStrategy = {one_for_one, 1000000, 1},

    Observer = ?CHILD(worker, life_observer, [X, Y]),
    Cells = [spec_cell(Datum) || Datum <- CellData],
    Time = ?CHILD(worker, life_time, [X, Y, CellNames]),

    Children = [Observer | Cells ++ [Time]],

    {ok, {RestartStrategy, Children}}.


spec_cell({_, Name, _}=Datum) ->
    M = life_cell,
    F = start_link,
    A = [Datum],
    {Name, {M, F, A}, permanent, 5000, worker, [M]}.
