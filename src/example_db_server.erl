-module(example_db_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([get_table/1, update_table/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link() ->
    gen_server:start_link({local, db_server}, example_db_server, maps:new(), []).

get_table(TableName) ->
    gen_server:call(db_server, {get_table, TableName}).

update_table(TableName, NewTable) ->
    gen_server:cast(db_server, {update_table, TableName, NewTable}).

init(Database) -> {ok, Database}.

handle_call({get_table, TableName}, _From, Database) ->
    case maps:find(TableName, Database) of
        {ok, Table} ->
            {reply, Table, Database};
        error ->
            Table = maps:new(),
            NewDatabase = maps:put(TableName, Table, Database),
            {reply, Table, NewDatabase}
    end.

handle_cast({update_table, TableName, NewTable}, OldDatabase) ->
    NewDatabase = maps:put(TableName, NewTable, OldDatabase),
    {noreply, NewDatabase}.

terminate(_, _) -> ok.

code_change(_, _, _) -> ok.

handle_info(_, State) -> {noreply, State}.