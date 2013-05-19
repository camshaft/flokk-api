%% @private
-module (flokk_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  configure(flokk_util:env("ERL_ENV", "production")),
  Port = list_to_integer(flokk_util:env("PORT", "5000")),

  Secret = simple_secrets:init(list_to_binary(flokk_util:env("ACCESS_TOKEN_KEY"))),
  ScopeEnum = jsx:decode(list_to_binary(flokk_util:env("SCOPES", "{}"))),

  Routes = flokk_util:load_dispatch(?MODULE),

  {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
    {compress, true},
    {env, [
      {dispatch, cowboy_router:compile(Routes)},
      {token_secret, Secret},
      {scopes_enum, ScopeEnum}
    ]},
    {onrequest, fun flokk_hook:start/1},
    {onresponse, fun flokk_hook:terminate/4},
    {middlewares, [
      flokk_middleware_empty_favicon,
      flokk_middleware_base,
      flokk_auth,
      cowboy_router,
      cowboy_handler,
      flokk_middleware_pubsub
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
