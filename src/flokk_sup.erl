%% @private
-module (flokk_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
  Procs = [
    {flokk_category,
      {flokk_category, start_link, []},
      permanent, 5000, worker, [flokk_category]},
    {flokk_product,
      {flokk_product, start_link, []},
      permanent, 5000, worker, [flokk_product]},
    {flokk_watcher,
      {flokk_watcher, start_link, []},
      permanent, 5000, worker, [flokk_watcher]}
  ],
  {ok, {{one_for_one, 10, 10}, Procs}}.
