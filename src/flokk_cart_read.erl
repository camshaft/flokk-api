-module (flokk_cart_read).

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
  case flokk_cart:read(ID) of
    {error, _} -> {error, 500, Req};
    {ok, Cart} -> {Cart, Req, State};
    _ -> {error, 404, Req}
  end.

body(ID, Cart, Req, State) ->
  URL = cowboy_base:resolve([<<"carts">>, ID], Req),

  {FormattedItems, Count} = format_item(Cart, URL, Req, 0, []),

  Body = [
    {<<"offer">>, FormattedItems},
    {<<"count">>, Count}
  ],

  %% Only show the checkout page if there's an item in the cart
  Body2 = case Count of
    0 ->
      Body;
    _ ->
      cowboy_resource_builder:authorize(<<"cart.checkout">>, Req, Body, [
        {<<"checkout">>, [
          {<<"action">>, URL},
          {<<"method">>, <<"POST">>},
          {<<"input">>, [
            {<<"shipping-address">>, [
              {<<"type">>, <<"select">>},
              {<<"prompt">>, <<"Shipping Address">>},
              {<<"options">>, [
                
              ]}
            ]},
            {<<"credit-card">>, [
              {<<"type">>, <<"select">>},
              {<<"prompt">>, <<"Credit Card">>},
              {<<"options">>, [
                
              ]}
            ]}
          ]}
        ]}
      ])
  end,

  UserID = cowboy_resource_owner:owner_id(Req),

  %% TODO check if we have the information we need for them to check out

  Body3 = cowboy_resource_builder:authorize(<<"cart.missing-info">>, Req, Body2, [
    {<<"shipping">>, [
      {<<"action">>, cowboy_base:resolve([<<"accounts">>, UserID], Req)},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        {<<"description">>, [
          {<<"type">>, <<"text">>}
        ]},
        {<<"name">>, [
          {<<"type">>, <<"text">>}
        ]},
        {<<"streetAddress">>, [
          {<<"type">>, <<"text">>}
        ]},
        {<<"addressLocality">>, [
          {<<"type">>, <<"text">>}
        ]},
        {<<"addressRegion">>, [
          {<<"type">>, <<"text">>}
        ]},
        {<<"postalCode">>, [
          {<<"type">>, <<"text">>}
        ]},
        {<<"addressCountry">>, [
          {<<"type">>, <<"text">>}
        ]}
      ]}
    ]},
    {<<"billing">>, [
      {<<"action">>, cowboy_base:resolve([<<"accounts">>, UserID], Req)},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        {<<"credit-card">>, [
          {<<"type">>, <<"x-balanced-uri">>}
        ]}
      ]}
    ]}
  ]),

  {Body3, Req, State}.

format_item([], _, _, Count, Acc) ->
  {Acc, Count};
format_item([{_, undefined}|Items], URL, Req, Count, Acc) ->
  format_item(Items, URL, Req, Count, Acc);
format_item([{Offer, Quantity}|Items], URL, Req, Count, Acc) ->
  Body = [
    {<<"quantity">>, Quantity},
    {<<"href">>, Offer}
  ],

  Acc2 = [cowboy_resource_builder:authorize(<<"cart.update">>, Req, Body, [
    {<<"remove">>, [
      {<<"action">>, URL},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        {<<"quantity">>, [
          {<<"type">>, <<"hidden">>},
          {<<"value">>, 0},
          {<<"prompt">>, <<"Quantity">>}
        ]},
        {<<"offer">>, [
          {<<"type">>, <<"hidden">>},
          {<<"value">>, Offer},
          {<<"prompt">>, <<"Quantity">>}
        ]}
      ]}
    ]}
  ])|Acc],

  format_item(Items, URL, Req, Count+Quantity, Acc2).

ttl(Req, State) ->
  {30, Req, State}.
