-module (flokk_client_read).

-export([init/2]).
-export([read/3]).
-export([body/4]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

read(ID, Req, State) ->
  case flokk_client:read(ID) of
    {error, _} -> {error, 500, Req};
    {ok, Client} -> {Client, Req, State};
    _ -> {error, 404, Req}
  end.

body(ID, Client, Req, State) ->

  Body = [
    {<<"name">>, fast_key:get(<<"name">>, Client, null)},
    {<<"description">>, fast_key:get(<<"description">>, Client, null)}
  ],

  Body2 = cowboy_resource_builder:authorize(<<"client.id">>, Req, Body, [
    {<<"id">>, ID},
    {<<"internal">>, fast_key:get(<<"internal">>, Client, false)}
  ]),

  Body3 = cowboy_resource_builder:authorize(<<"client.secret">>, Req, Body2, [
    {<<"secret">>, fast_key:get(<<"secret">>, Client, null)}
  ]),

  Body4 = cowboy_resource_builder:authorize(<<"client.redirect_uri">>, Req, Body3, [
    {<<"redirect_uri">>, fast_key:get(<<"redirect_uri">>, Client, null)}
  ]),

  Body5 = cowboy_resource_builder:authorize(<<"client.scopes">>, Req, Body4, [
    {<<"scopes">>, fast_key:get(<<"scopes">>, Client, null)}
  ]),

  io:format("Client Output ~p~n", [Body5]),

  {Body5, Req, State}.

ttl(Req, State) ->
  {3600, Req, State}.
