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
  case flokk_user:create(Body) of
    {ok, ID} ->
      {ID, Req, State};
    {error, email_in_use} ->
      {error, 400, Req};
    {error, _Error} ->
      {error, 500, Req}
  end.

location(ID, Req, State) ->
  {cowboy_base:resolve([<<"users">>, ID], Req), Req, State}.
