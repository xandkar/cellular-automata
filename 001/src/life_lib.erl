-module(life_lib).

-export([cast_one2all/2
        ,cast_all2one/2
        ]).


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

