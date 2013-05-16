%% @private
-module (flokk_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  configure(flokk_util:env("ERL_ENV", "production")),
  Routes = flokk_util:load_dispatch(?MODULE),
  Dispatch = cowboy_router:compile(Routes),
  Port = list_to_integer(flokk_util:env("PORT", "5000")),
  {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
    {compress, true},
    {env, [{dispatch, Dispatch}]},
    {onresponse, fun flokk_hook:handle/4},
    {middlewares, [
      flokk_middleware_empty_favicon,
      flokk_middleware_vary, %% TODO do we really need this?
      flokk_middleware_base,
      flokk_middleware_auth,
      flokk_middleware_method_match,
      cowboy_router,
      flokk_middleware_available,
      flokk_middleware_scope,
      cowboy_handler,
      flokk_middleware_ttl,
      flokk_middleware_etag,
      flokk_middleware_json,
      flokk_middleware_pubsub
    ]}
  ]),
  lager:info("Server started on port ~p", [flokk_util:env("PORT")]),
  flokk_sup:start_link().

stop(_State) ->
  ok.

configure("development") ->
  sync:go(),
  lager:set_loglevel(lager_console_backend, debug);
configure(_)->
  ok.
