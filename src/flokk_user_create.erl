-module(flokk_user_create).

-export([init/2]).
-export([scope/2]).
-export([create/3]).
-export([location/3]).

-define(SCOPE, <<"user.create">>).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

create(Body, Req, State) ->
  Response = flokk_user:create(Body, cowboy_env:get(Req)),
  {Response, Req, State}.

location(ID, Req, State) ->
  {cowboy_base:resolve([<<"users">>, ID], Req), Req, State}.
