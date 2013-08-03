-module(flokk_root).

-export([init/2]).
-export([body/2]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

body(Req, State) ->
  P = presenterl:create(),

  P ! [
    % {<<"sales">>, [
    %   {<<"href">>, cowboy_base:resolve(<<"sales">>, Req)},
    %   {<<"title">>, <<"Sales">>}
    % ]},
    {<<"categories">>, [
      {<<"href">>, cowboy_base:resolve(<<"categories">>, Req)},
      {<<"title">>, <<"Categories">>}
    ]},
    {<<"vendors">>, [
      {<<"href">>, cowboy_base:resolve(<<"vendors">>, Req)},
      {<<"title">>, <<"Vendors">>}
    ]}
  ],

  %% User specific links
  OwnerID = cowboy_resource_owner:owner_id(Req),

  presenterl:conditional([
    OwnerID =/= undefined
  ], fun() ->
    [
      {<<"account">>, [
        {<<"href">>, cowboy_base:resolve([<<"users">>, OwnerID], Req)}
      ]}
    ]
  end, P),

  presenterl:conditional([
    cowboy_resource_owner:is_authorized(<<"cart.read">>, Req)
  ], fun() ->
    [
      {<<"cart">>, [
        {<<"href">>, cowboy_base:resolve([<<"carts">>, OwnerID], Req)}
      ]}
    ]
  end, P),

  %% Auth links
  presenterl:conditional([
    cowboy_resource_owner:is_authorized(<<"user">>, Req)
  ], [
    {<<"users">>, [
      {<<"href">>, cowboy_base:resolve([<<"users">>], Req)}
    ]}
  ], P),
  presenterl:conditional([
    cowboy_resource_owner:is_authorized(<<"client">>, Req)
  ], [
    {<<"clients">>, [
      {<<"href">>, cowboy_base:resolve([<<"clients">>], Req)}
    ]}
  ], P),

  %% Item listing
  presenterl:conditional([
    cowboy_resource_owner:is_authorized(<<"item.list">>, Req)
  ], [
    {<<"items">>, [
      {<<"href">>, cowboy_base:resolve([<<"items">>], Req)}
    ]}
  ], P),

  Body = presenterl:encode(P),

  {Body, Req, State}.

ttl(Req, State)->
  {3600, Req, State}.
