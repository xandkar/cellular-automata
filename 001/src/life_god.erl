-module(life_god).
-behaviour(supervisor).


%% API
-export([start_link/2]).

%% Callbacks
-export([init/1]).


%% Helper macro for declaring children of supervisor
-define(CHILD(Type, I, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).


%% ============================================================================
%% API
%% ============================================================================

start_link(X, CellData) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [X, CellData]).


%% ============================================================================
%% Callbacks
%% ============================================================================

init([X, CellData]) ->
    CellNames = [Name || {_, Name, _} <- CellData],
    RestartStrategy = {one_for_one, 5, 10},
    Cells = [spec_cell(Datum) || Datum <- CellData],
    Time = ?CHILD(worker, life_time, [X, CellNames]),
    Children = Cells ++ [Time],
    {ok, {RestartStrategy, Children}}.


spec_cell({_, Name, _}=Datum) ->
    M = life_cell,
    F = start_link,
    A = [Datum],
    {Name, {M, F, A}, permanent, 5000, worker, [M]}.

    %{ID, {ID, start_link, Args}, permanent, 5000, Type, [ID]}.




