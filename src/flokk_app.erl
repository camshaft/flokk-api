%% @private
-module(flokk_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  ENV = simple_env:get("ERL_ENV", "production"),

  RiakURL = simple_env:get_binary("RIAK_URL", <<"riak://localhost">>),
  Min = simple_env:get_integer("RIAK_POOL_MIN", 50),
  Max = simple_env:get_integer("RIAK_POOL_MAX", 500),
  ok = riakou:start_link(RiakURL, [], Min, Max),

  configure(ENV),

  Secret = simple_secrets:init(simple_env:get_binary("ACCESS_TOKEN_KEY")),
  ScopeEnum = binary:split(simple_env:get_binary("SCOPES", <<>>), <<",">>, [global]),

  Routes = flokk_util:load_dispatch(?MODULE),

  Listeners = simple_env:get_integer("NUM_LISTENERS", 100),
  Port = simple_env:get_integer("PORT", 5000),

  {ok, _} = cowboy:start_http(http, Listeners, [{port, Port}], [
    {compress, true},
    {env, [
      {dispatch, cowboy_router:compile(Routes)},
      {ss_token_secret, Secret},
      {ss_scopes_enum, ScopeEnum},
      {token_handler, cowboy_resource_owner_simple_secrets},
      {cors, []}
    ]},
    {onrequest, fun flokk_hook:start/1},
    {onresponse, fun flokk_hook:terminate/4},
    {middlewares, [
      cowboy_env,
      cowboy_cors,
      cowboy_empty_favicon,
      cowboy_base,
      cowboy_resource_owner,
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
