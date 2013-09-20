%% @private
-module(flokk_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  ok = riakou:start_link(simple_env:get_binary("RIAK_URL", <<"riak://localhost">>)),
  ok = riakou:wait_for_connection(),

  erlenv:configure(fun configure/1),

  Secret = simple_env:get_binary("ACCESS_TOKEN_KEY"),
  ScopeEnum = binary:split(simple_env:get_binary("SCOPES", <<>>), <<",">>, [global]),

  {ok, _} = cowboy:start_http(http, simple_env:get_integer("NUM_LISTENERS", 100), [
    {port, Port = simple_env:get_integer("PORT", 5000)}
  ], [
    {compress, true},
    {env, [
      {dispatch, cowboy_route_loader:compile(flokk)}
    ]},
    {onrequest, fun flokk_hook:start/1},
    {onresponse, fun flokk_hook:terminate/4},
    {middlewares, [
      cowboy_env,
      {cowboy_fun, cowboy_cors:init([handle_options])},
      cowboy_empty_favicon,
      {cowboy_fun, cowboy_base:init()},
      {cowboy_fun, cowboy_resource_owner:init(cowboy_resource_owner_simple_secrets:init(Secret, ScopeEnum))},
      cowboy_router,
      cowboy_handler,
      cowboy_pusher
    ]}
  ]),

  lager:info("Server started on port ~p", [Port]),

  gen_batch_sup:start_link(),
  simple_sup:start_link([
    flokk_client,
    {pusherl, start_link, [simple_env:get_binary("PUSHER_URL")]},
    {flokk_item_scoreboard, start_link, [simple_env:get("SCOREBOARD_URL")]}
  ]).

stop(_State) ->
  ok.

configure("development") ->
  sync:go();
configure(_)->
  ok.
