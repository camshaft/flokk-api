-module(flokk_vendor_create).

-export([init/2]).
-export([scope/2]).
-export([create/3]).
-export([location/3]).

-define(SCOPE, <<"vendor.create">>).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

create(Body, Req, State) ->
  Result = flokk_vendor:create(Body, cowboy_env:get(Req)),
  {Result, Req, State}.

location(ID, Req, State) ->
  {cowboy_base:resolve([<<"vendors">>, ID], Req), Req, State}.
