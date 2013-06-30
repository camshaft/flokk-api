-module (flokk_client_read).

-export([init/2]).
-export([read/3]).
-export([body/4]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

read(ID, Req, State) ->
  case flokk_client:read(ID) of
    {error, notfound} -> {error, 404, Req};
    {error, _} -> {error, 500, Req};
    {ok, Client} -> {Client, Req, State}
  end.

body(ID, Client, Req, State) ->

  Body = [
    {<<"name">>, fast_key:get(<<"name">>, Client)},
    {<<"description">>, fast_key:get(<<"description">>, Client)}
  ],

  Body2 = cowboy_resource_builder:authorize(<<"client.id">>, Req, Body, [
    {<<"id">>, ID}
  ]),

  Body3 = cowboy_resource_builder:authorize(<<"client.secret">>, Req, Body2, [
    {<<"secret">>, fast_key:get(<<"secret">>, Client)}
  ]),

  Body4 = cowboy_resource_builder:authorize(<<"client.redirect_uri">>, Req, Body3, [
    {<<"redirect_uri">>, fast_key:get(<<"redirect_uri">>, Client)}
  ]),

  Body5 = cowboy_resource_builder:authorize(<<"client.scopes">>, Req, Body4, [
    {<<"scopes">>, fast_key:get(<<"scopes">>, Client)}
  ]),

  {Body5, Req, State}.

ttl(Req, State) ->
  {3600, Req, State}.
