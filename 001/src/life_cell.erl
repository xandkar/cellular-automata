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


-record(state, {cell_id         :: integer()
               ,name            :: string()
               ,cell_state      :: 0 | 1
               ,neighbors       :: list(atom())
               ,live_neighbors  :: integer()
               ,num_neighbors   :: integer()
               ,replies_pending :: integer()
               ,gen_id          :: integer()
               ,early_msgs      :: list()
               }).


%% ============================================================================
%% API
%% ============================================================================

start_link({_, Name, _}=Datum) ->
    ServerName = {local, Name},
    Args = [Datum],
    Opts = [],
    gen_server:start_link(ServerName, ?MODULE, Args, Opts).


%% ============================================================================
%% Callbacks
%% ============================================================================

init([{CellID, Name, NeighborNames}]) ->
    State = #state{cell_id         = CellID
                  ,name            = Name
                  ,cell_state      = crypto:rand_uniform(0, 2)
                  ,neighbors       = NeighborNames
                  ,num_neighbors   = length(NeighborNames)
                  ,live_neighbors  = 0
                  ,replies_pending = 0
                  ,early_msgs      = []
                  },
    {ok, State}.


terminate(_Reason, State) ->
    {ok, State}.


code_change(_Old, State, _Other) ->
    {ok, State}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast({next_gen, GenID},
    #state{name=Name
          ,cell_state=CellState
          ,neighbors=Neighbors
          ,num_neighbors=NumNeighbors
          ,early_msgs=EarlyMsgs
          }=State) ->

    ok = cast_one2all(Neighbors, {state_broadcast, GenID, CellState}),

    % Put stashed messages back in the mailbox,
    % now that we're ready to handle them
    ok = cast_all2one(Name, EarlyMsgs),

    NewState = State#state{replies_pending=NumNeighbors
                          ,gen_id=GenID
                          ,early_msgs=[]
                          },

    {noreply, NewState};


%% If we receive 'state_broadcast' before we receive 'next_gen',
%% stash it until we do.
%%
%% Took me a while to realize this, but sometimes it is possible. The more
%% there're cells, the more likely this is to happen.
%%
handle_cast({state_broadcast, ReceivedGenID, _NeighborState}=Msg,
    #state{gen_id=GenID
          ,early_msgs=EarlyMsgs
          }=State) when GenID =/= ReceivedGenID ->

    {noreply, State#state{early_msgs=[Msg|EarlyMsgs]}};


%% Now that we can be sure that this request is for the current generation, we
%% can handle it
handle_cast({state_broadcast, GenID, NeighborState},
    #state{cell_id=CellID
          ,gen_id=GenID
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
            ok = life_time:report_state(CellID, GenID, NewCellState),

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

% Cast all messages to one destination
cast_all2one(_, []) -> ok;
cast_all2one(Server, [Msg | Msgs]) ->
    ok = gen_server:cast(Server, Msg),
    cast_all2one(Server, Msgs).


% Cast one message to all destinations
cast_one2all([], _) -> ok;
cast_one2all([Server | Servers], Msg) ->
    ok = gen_server:cast(Server, Msg),
    cast_one2all(Servers, Msg).


new_state(1, LiveNeighbors) when LiveNeighbors  <  2 -> 0;
new_state(1, LiveNeighbors) when LiveNeighbors  <  4 -> 1;
new_state(1, LiveNeighbors) when LiveNeighbors  >  3 -> 0;
new_state(0, LiveNeighbors) when LiveNeighbors =:= 3 -> 1;
new_state(State, _LiveNeighbors) -> State.
