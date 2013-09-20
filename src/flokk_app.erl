%% @private
-module(flokk_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  RiakURL = simple_env:get_binary("RIAK_URL", <<"riak://localhost">>),
  Min = simple_env:get_integer("RIAK_POOL_MIN", 50),
  Max = simple_env:get_integer("RIAK_POOL_MAX", 500),
  ok = riakou:start_link(RiakURL, [], Min, Max),

  wait_for_riak(riakou:do(ping, [])).

wait_for_riak(pong) ->
  listen();
wait_for_riak(_) ->
  timer:sleep(5),
  wait_for_riak(riakou:do(ping, [])).

listen() ->
  erlenv:configure(fun configure/1),

  Secret = simple_env:get_binary("ACCESS_TOKEN_KEY"),
  ScopeEnum = binary:split(simple_env:get_binary("SCOPES", <<>>), <<",">>, [global]),

  {ok, _} = cowboy:start_http(http, Port = simple_env:get_integer("PORT", 5000), [
    {port, simple_env:get_integer("PORT", 5000)}
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
  flokk_sup:start_link().

stop(_State) ->
  ok.

configure("development") ->
  sync:go();
configure(_)->
  ok.
