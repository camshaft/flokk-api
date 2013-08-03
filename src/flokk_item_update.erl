-module(flokk_item_update).

-export([init/2]).
-export([scope/2]).
-export([update/4]).

-define(SCOPE, <<"item.update">>).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

update(ID, Body, Req, State) ->
  Response = flokk_item:update(ID, Body, cowboy_env:get(Req)),
  {Response, Req, State}.
