-module(flokk_vendor_update).

-export([init/2]).
-export([scope/2]).
-export([update/4]).

-define(SCOPE, <<"vendor.update">>).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

update(ID, Body, Req, State) ->
  Response = flokk_vendor:update(ID, Body, cowboy_env:get(Req)),
  {Response, Req, State}.
