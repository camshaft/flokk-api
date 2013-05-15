%% @private
-module (flokk_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  configure(flokk_util:env("ERL_ENV", "production")),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", flokk_root_handler, []},
      {"/categories", flokk_categories_handler, []},
      {"/categories/:category", flokk_category_handler, []},
      {"/cart/:cart", flokk_cart_handler, []},
      {"/products", flokk_products_handler, []},
      {"/products/:product", flokk_product_handler, []},
      {"/products/:product/watchers", flokk_watchers_handler, []},
      {"/users/:user", flokk_user_handler, []},
      {"/users/:user/history", flokk_user_history_handler, []}
    ]}
  ]),
  Port = list_to_integer(flokk_util:env("PORT", "5000")),
  {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
    {compress, true},
    {env, [{dispatch, Dispatch}]},
    {onresponse, fun flokk_response:handle/4}, %% TODO only enable in dev
    {middlewares, [
      flokk_middleware_empty_favicon,
      flokk_middleware_vary,
      flokk_middleware_base,
      flokk_middleware_auth,
      cowboy_router,
      cowboy_handler
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
