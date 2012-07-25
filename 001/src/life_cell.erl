-module(life_cell).
-behaviour(gen_server).


%% API
-export([start_link/1]).

%% Callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).


-record(state, {id              :: integer()
               ,name            :: string()
               ,cell_state      :: 0 | 1
               ,neighbors       :: list(atom())
               ,live_neighbors  :: integer()
               ,num_neighbors   :: integer()
               ,replies_pending :: integer()
               ,generation      :: integer()
               }).


%% ============================================================================
%% API
%% ============================================================================

start_link({_ID, Name, _NeighborNames}=Datum) ->
    ServerName = {local, Name},
    Args = [Datum],
    Opts = [],
    gen_server:start_link(ServerName, ?MODULE, Args, Opts).


%% ============================================================================
%% Callbacks
%% ============================================================================

init([{ID, Name, NeighborNames}]) ->
    State = #state{id=ID
                  ,name=Name
                  ,cell_state=crypto:rand_uniform(0, 2)
                  ,neighbors=NeighborNames
                  ,num_neighbors=length(NeighborNames)
                  ,live_neighbors=0
                  ,replies_pending=0
                  },
    {ok, State}.


terminate(_Reason, State) ->
    {ok, State}.


code_change(_Old, State, _Other) ->
    {ok, State}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast({tick, Generation},
    #state{name=Name
          ,neighbors=Neighbors
          ,num_neighbors=NumNeighbors
          }=State) ->
    ok = cast_all(Neighbors, {request_state, Name}),
    {noreply, State#state{replies_pending=NumNeighbors, generation=Generation}};


handle_cast({request_state, Requester}, State) ->
    ok = gen_server:cast(Requester, {response_state, State#state.cell_state}),
    {noreply, State};


handle_cast({response_state, NeighborState},
    #state{id=ID
          ,replies_pending=Pending
          ,cell_state=CellState
          ,live_neighbors=LiveNeighbors
          }=State) ->

    NewPending = Pending - 1,
    NewLiveNeighbors = LiveNeighbors + NeighborState,

    NewState = State#state{replies_pending=NewPending
                          ,live_neighbors=NewLiveNeighbors
                          },

    case NewPending of
        0 ->
            NewCellState = new_state(CellState, NewLiveNeighbors),
            ok = life_time:tock(ID, NewCellState),

            {noreply, NewState#state{live_neighbors=0
                                    ,cell_state=NewCellState
                                    }
            };

        _N ->
            {noreply, NewState}
    end;


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Msg, State) ->
    {noreply, State}.


%% ============================================================================
%% Internal
%% ============================================================================

cast_all([], _) -> ok;
cast_all([Server | Servers], Msg) ->
    ok = gen_server:cast(Server, Msg),
    cast_all(Servers, Msg).


new_state(1, LiveNeighbors) when LiveNeighbors  <  2 -> 0;
new_state(1, LiveNeighbors) when LiveNeighbors  <  4 -> 1;
new_state(1, LiveNeighbors) when LiveNeighbors  >  3 -> 0;
new_state(0, LiveNeighbors) when LiveNeighbors =:= 3 -> 1;
new_state(State, _LiveNeighbors) -> State.
