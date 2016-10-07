-module(example_db_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        {
            db_server,
            {example_db_server, start_link, []},
            permanent, 5000, worker, [example_db_server]
        },
        {
            db_worker_sup,
            {example_db_worker_sup, start_link, []},
            permanent, 5000, supervisor, [example_db_worker_sup]
        }
    ]}}.

