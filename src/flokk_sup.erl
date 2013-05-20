%% @private
-module (flokk_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).
-export([start_link/1]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [simple_db_riak]).
start_link(Backend) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Backend]).

%% supervisor.

init([Backend]) ->
  Procs = [
    {flokk_category,
      {flokk_category, start_link, [Backend]},
      permanent, 5000, worker, [flokk_category]},
    {flokk_item,
      {flokk_item, start_link, [Backend]},
      permanent, 5000, worker, [flokk_item]}
  ],
  {ok, {{one_for_one, 10, 10}, Procs}}.
