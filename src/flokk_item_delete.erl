-module(flokk_item_delete).

-export([init/2]).
-export([scope/2]).
-export([delete/3]).
-export([location/2]).

-define(SCOPE, <<"item.delete">>).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

delete(ID, Req, State) ->
  Response = flokk_item:delete(ID, cowboy_env:get(Req)),
  {Response, Req, State}.

location(Req, State) ->
  {cowboy_base:resolve(<<"item">>, Req), Req, State}.
