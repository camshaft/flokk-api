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
  supervisor:start_link({local, ?MODULE}, ?MODULE, [flokk_db_riak]).
start_link(Backend) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Backend]).

%% supervisor.

init([Backend]) ->
  RiakUrl = get_riak_url(),
  PusherURL = simple_env:get_binary("PUSHER_URL"),
  ScoreboardURL = simple_env:get_binary("SCOREBOARD_URL"),

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
      permanent, 5000, worker, [flokk_item_scoreboard]},
    %% Database
    {Backend,
      {Backend, start_link, [RiakUrl]},
      permanent, 5000, worker, [Backend]}
  ],
  {ok, {{one_for_one, 10, 10}, Procs}}.

%% TODO make this into a lib

get_riak_url()->
  Urls = binary:split(simple_env:get_binary("RIAK_URL", <<"riak://127.0.0.1:8087">>), <<",">>, [global]),
  [parse_riak_url(Url) || Url <- Urls].

parse_riak_url(<<"riak://",Url/binary>>)->
  [Host, Port] = binary:split(Url, <<":">>),
  {binary_to_list(Host), list_to_integer(binary_to_list(Port))}.
