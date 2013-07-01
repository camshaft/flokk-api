-module (flokk_user_read).

-export([init/2]).
-export([read/3]).
-export([body/4]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

read(ID, Req, State) ->
  case flokk_user:read(ID) of
    {error, notfound} -> {error, 404, Req};
    {error, _} -> {error, 500, Req};
    {ok, User} -> {User, Req, State}
  end.

body(ID, User, Req, State) ->

  Body = [],

  Body2 = cowboy_resource_builder:authorize(<<"user.id">>, Req, Body, [
    {<<"id">>, ID}
  ]),

  Body3 = cowboy_resource_builder:authorize(<<"user.email">>, Req, Body2, [
    {<<"email">>, fast_key:get(<<"email">>, User)}
  ]),

  Body4 = cowboy_resource_builder:authorize(<<"user.passhash">>, Req, Body3, [
    {<<"passhash">>, fast_key:get(<<"passhash">>, User)}
  ]),

  Body5 = cowboy_resource_builder:authorize(<<"user.name">>, Req, Body4, [
    {<<"name">>, fast_key:get(<<"name">>, User)}
  ]),

  {Body5, Req, State}.

ttl(Req, State) ->
  {3600, Req, State}.
