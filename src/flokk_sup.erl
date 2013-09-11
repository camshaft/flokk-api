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
  PusherURL = simple_env:get_binary("PUSHER_URL"),
  ScoreboardURL = simple_env:get("SCOREBOARD_URL"),

  gen_batch_sup:start_link(),

  Procs = [
    %% Store the clients in code for now
    {flokk_client,
      {flokk_client, start_link, []},
      permanent, 5000, worker, [flokk_client]},
    %% Hook to pusher
    {pusherl,
      {pusherl, start_link, [PusherURL]},
      permanent, 5000, worker, [pusherl]},
    {flokk_item_scoreboard,
      {flokk_item_scoreboard, start_link, [ScoreboardURL]},
      permanent, 5000, worker, [flokk_item_scoreboard]}
  ],
  {ok, {{one_for_one, 10, 10}, Procs}}.

