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
  P = presenterl:create(),

  P ! [
    {<<"name">>, fast_key:get(<<"name">>, Client)},
    {<<"description">>, fast_key:get(<<"description">>, Client)}
  ],

  presenterl:conditional([
    cowboy_resource_owner:is_authorized([<<"client.id">>], Req)
  ], [
    {<<"id">>, ID}
  ], P),

  presenterl:conditional([
    cowboy_resource_owner:is_authorized([<<"client.internal">>], Req)
  ], [
    {<<"internal">>, fast_key:get(<<"internal">>, Client, false)}
  ], P),

  [presenterl:conditional([
    cowboy_resource_owner:is_authorized([<<"client.", Prop/binary>>], Req)
  ], [
    {Prop, fast_key:get(Prop, Client)}
  ], P) || Prop <- [<<"secret">>, <<"redirect_uri">>, <<"scopes">>]],

  Body = presenterl:encode(P),

  {Body, Req, State}.

ttl(Req, State) ->
  {3600, Req, State}.
