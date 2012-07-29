-module(life_observer).
-behaviour(gen_event).


%% API
-export([start_link/2
        ,register_with_logger/0
        ,add_handler/2
        ,delete_handler/0
        ,log_generation/3
        ]).

%% Callbacks
-export([init/1
        ,terminate/2
        ,code_change/3
        ,handle_event/2
        ,handle_call/2
        ,handle_info/2
        ]).


-define(EVENT_MGR_REF, ?MODULE).
-define(HANDLER, ?MODULE).

-define(PATH_DIR__DATA, "data").
-define(CSV_DELIMITER, ",").


-record(state, {log_file :: file:io_device()}).


%% ============================================================================
%% API
%% ============================================================================

start_link(X, Y) ->
    EventMgrName = {local, ?EVENT_MGR_REF},
    {ok, PID} = gen_event:start_link(EventMgrName),
    ok = add_handler(X, Y),
    {ok, PID}.


register_with_logger() ->
    error_logger:add_report_handler(?HANDLER).


add_handler(X, Y) ->
    Args = [X, Y],
    gen_event:add_handler(?EVENT_MGR_REF, ?HANDLER, Args).


delete_handler() ->
    Args = [],
    gen_event:delete_handler(?EVENT_MGR_REF, ?HANDLER, Args).


log_generation(GenID, Dead, Alive) ->
    Event = {generation, GenID, Dead, Alive},
    gen_event:notify(?EVENT_MGR_REF, Event).


%% ============================================================================
%% Callbacks (unused)
%% ============================================================================

code_change(_Old, State, _Other) -> {ok, State}.
handle_call(_Request, State)     -> Reply = ok, {ok, Reply, State}.
handle_info(_Info, State)        -> {ok, State}.


%% ============================================================================
%% Callbacks
%% ============================================================================

init([X, Y]) ->
    N = X * Y,
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    DateTime = lists:flatten(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0B--~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]
    )),

    FileNameComponents = [DateTime | [integer_to_list(I) || I <- [N, X, Y]]],
    FileName = string:join(FileNameComponents, "--") ++ ".csv",
    FilePath = filename:join(?PATH_DIR__DATA, FileName),

    ok = validate_makedir(file:make_dir(?PATH_DIR__DATA)),
    {ok, LogFile} = file:open(FilePath, write),

    CSVHeaders = ["Generation", "Dead", "Alive"],
    ok = do_write(LogFile, list_to_csvline(CSVHeaders)),

    {ok, #state{log_file=LogFile}}.


terminate(_Reason, #state{log_file=LogFile}=State) ->
    file:close(LogFile),
    {ok, State}.


handle_event({generation, GenID, Dead, Alive},
    #state{log_file=LogFile}=State) ->

    Values = [integer_to_list(I) || I <- [GenID, Dead, Alive]],
    ok = do_write(LogFile, list_to_csvline(Values)),
    {ok, State}.


%% ============================================================================
%% Internal
%% ============================================================================

validate_makedir(ok) -> ok;
validate_makedir({error, eexist}) -> ok;
validate_makedir(Other) -> Other.


do_write(File, Line) ->
    ok = io:format(File, "~s~n", [Line]).


list_to_csvline(List) ->
    string:join(List, ?CSV_DELIMITER).
