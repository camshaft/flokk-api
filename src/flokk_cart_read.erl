-module(flokk_cart_read).

-export([init/2]).
-export([scope/2]).
-export([read/3]).
-export([body/4]).
-export([ttl/2]).

-define(SCOPE, [<<"cart.read">>]).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

read(ID, Req, State) ->
  Response = flokk_cart:read(ID, cowboy_env:get(Req)),
  {Response, Req, State}.

body(ID, Cart, Req, State) ->
  URL = cowboy_base:resolve([<<"carts">>, ID], Req),
  OwnerID = cowboy_resource_owner:owner_id(Req),

  {FormattedItems, Count} = format_item(Cart, URL, OwnerID, Req),

  P = presenterl:create(),

  P ! [
    {<<"offer">>, FormattedItems},
    {<<"count">>, Count}
  ],

  presenterl:conditional([
    Count =/= 0,
    cowboy_resource_owner:is_authorized(<<"cart.checkout">>, Req)
  ], [
    {<<"addresses">>, [
      {<<"href">>, cowboy_base:resolve([<<"users">>, OwnerID], Req)}
    ]},
    {<<"checkout">>, [
      {<<"action">>, cowboy_base:resolve([<<"carts">>, ID], Req)},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        {<<"action">>, [
          {<<"type">>, <<"hidden">>},
          {<<"value">>, <<"checkout">>}
        ]},
        {<<"name">>, [
          {<<"type">>, <<"text">>},
          {<<"prompt">>, <<"Recipient Name">>}
        ]},
        {<<"streetAddress">>, [
          {<<"type">>, <<"text">>},
          {<<"prompt">>, <<"Street">>}
        ]},
        {<<"addressLocality">>, [
          {<<"type">>, <<"text">>},
          {<<"prompt">>, <<"City/Locality">>}
        ]},
        {<<"addressRegion">>, [
          {<<"type">>, <<"text">>},
          {<<"prompt">>, <<"State/Region">>}
        ]},
        {<<"postalCode">>, [
          {<<"type">>, <<"text">>},
          {<<"prompt">>, <<"Postal Code">>}
        ]},
        {<<"addressCountry">>, [
          {<<"type">>, <<"text">>},
          {<<"prompt">>, <<"Country">>}
        ]},
        {<<"creditCard">>, [
          {<<"type">>, <<"x-balanced-uri">>},
          {<<"prompt">>, <<"Credit Card">>}
        ]}
      ]}
    ]}
  ], P),

  Body = presenterl:encode(P),

  {Body, Req, State}.

format_item(Items, URL, OwnerID, Req) ->
  P = presenterl:create(),
  Count = format_item(Items, URL, Req, 0, OwnerID, P),
  {presenterl:encode(P), Count}.

format_item([], _, _, Count, _, _) ->
  Count;
format_item([{Item, Quantity}|Items], URL, Req, Count, OwnerID, P) when is_integer(Quantity) ->
  P ! {add, cowboy_resource_builder:authorize([OwnerID, undefined], <<"cart.update">>, Req, [
    {<<"quantity">>, Quantity},
    {<<"href">>, cowboy_base:resolve([<<"items">>, Item], Req)}
  ], [
    {<<"update">>, [
      {<<"action">>, URL},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        {<<"action">>, [
          {<<"type">>, <<"hidden">>},
          {<<"value">>, <<"update">>}
        ]},
        {<<"prev-quantity">>, [
          {<<"type">>, <<"hidden">>},
          {<<"value">>, Quantity}
        ]},
        {<<"offer">>, [
          {<<"type">>, <<"hidden">>},
          {<<"value">>, Item}
        ]},
        {<<"quantity">>, [
          {<<"type">>, <<"range">>},
          {<<"value">>, Quantity},
          {<<"prompt">>, <<"Quantity">>}
        ]}
      ]}
    ]},
    {<<"remove">>, [
      {<<"action">>, URL},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        {<<"action">>, [
          {<<"type">>, <<"hidden">>},
          {<<"value">>, <<"remove">>}
        ]},
        {<<"offer">>, [
          {<<"type">>, <<"hidden">>},
          {<<"value">>, Item},
          {<<"prompt">>, <<"Quantity">>}
        ]}
      ]}
    ]}
  ])},
  format_item(Items, URL, Req, Count+Quantity, OwnerID, P);
format_item([{_, _}|Items], URL, Req, Count, OwnerID, P) ->
  format_item(Items, URL, Req, Count, OwnerID, P).

ttl(Req, State) ->
  {30, Req, State}.
