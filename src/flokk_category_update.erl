-module(flokk_category_update).

-export([init/2]).
-export([scope/2]).
-export([update/4]).

-define(SCOPE, <<"category.update">>).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

update(ID, Body, Req, State) ->
  Response = flokk_category:update(ID, Body, cowboy_env:get(Req)),
  {Response, Req, State}.
