-module(flokk_item_create).

-export([init/2]).
-export([scope/2]).
-export([create/3]).
-export([location/3]).

-define(SCOPE, <<"item.create">>).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

create(Body, Req, State) ->
  Response = flokk_item:create(Body, cowboy_env:get(Req)),
  {Response, Req, State}.

location(ID, Req, State) ->
  {cowboy_base:resolve([<<"items">>, ID], Req), Req, State}.
