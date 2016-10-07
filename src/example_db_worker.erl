-module(example_db_worker).

-behaviour(gen_server).

-export([start_link/2]).
-export([find/3, insert/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(db_client, {name, client_pid}).

start_link(Name, ClientPid) ->
    gen_server:start_link(example_db_worker, {Name, ClientPid}, []).

init({Name, ClientPid}) -> {ok, #db_client{name=Name, client_pid=ClientPid}}.

find(Pid, TableName, Id) ->
    gen_server:call(Pid, {get, TableName, Id}).

insert(Pid, TableName, Id, Value) ->
    gen_server:cast(Pid, {insert, TableName, Id, Value}).

get_table(TableName) ->
    example_db_server:get_table(TableName).

update_table(TableName, NewTable) ->
    example_db_server:update_table(TableName, NewTable).

handle_call({get, TableName, Id}, _From, Client) ->
    Table = get_table(TableName),
    Result = maps:find(Id, Table),
    {reply, Result, Client}.

handle_cast({insert, TableName, Id, Value}, Client) ->
    OldTable = get_table(TableName),
    NewTable = maps:put(Id, Value, OldTable),
    update_table(TableName, NewTable),
    {noreply, Client}.

handle_info(_, State) -> {noreply, State}.

code_change(_, _, _) -> ok.

terminate(_Reason, _State) -> ok.