-module(flokk_user_delete).

-export([init/2]).
-export([scope/2]).
-export([delete/3]).
-export([location/3]).

-define(SCOPE, <<"user.delete">>).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

delete(ID, Req, State) ->
  Response = flokk_user:delete(ID, cowboy_env:get(Req)),
  {Response, Req, State}.

location(ID, Req, State) ->
  {cowboy_base:resolve([<<"users">>, ID], Req), Req, State}.
