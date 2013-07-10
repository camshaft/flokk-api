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

  Body = [
    {<<"offer">>, [
      format_item(Item, URL, Req) || Item <- Cart
    ]}
  ],

  Body2 = cowboy_resource_builder:authorize(<<"cart.checkout">>, Req, Body, [
    {<<"action">>, URL},
    {<<"method">>, <<"POST">>},
    {<<"input">>, []}
  ]),

  {Body2, Req, State}.

format_item({Offer, Quantity}, URL, Req) ->
  Body = [
    {<<"quantity">>, Quantity},
    {<<"offer">>, Offer}
  ],

  cowboy_resource_builder:authorize(<<"cart.update">>, Req, Body, [
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
  ]).

ttl(Req, State) ->
  {3600, Req, State}.
