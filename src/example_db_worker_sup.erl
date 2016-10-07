-module(example_db_worker_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([create_worker/2]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

create_worker(Name, ClientPid) ->
    supervisor:start_child(?MODULE, {
        make_ref(),
        {example_db_worker, start_link, [Name, ClientPid]},
        permanent, brutal_kill, worker, [example_db_worker]
    }).

%% Worker does have no children as default
init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

