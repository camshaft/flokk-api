-module(flokk_cart_action).

-export([init/2]).
-export([scope/2]).
-export([validate/3]).
-export([add/4]).
-export([remove/4]).
-export([checkout/4]).

-define(SCOPE, [<<"cart.update">>]).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

%% TODO validate body
validate(_Body, Req, State) ->
  {true, Req, State}.

add(ID, Body, Req, State) ->
  Offer = fast_key:get(<<"offer">>, Body),

  %% TODO we might need to parse these
  Quantity = fast_key:get(<<"quantity">>, Body),
  PrevQuantity = fast_key:get(<<"prev-quantity">>, Body),

  case flokk_cart:add(ID, Offer, Quantity - PrevQuantity) of
    {ok, Cart} ->
      UserID = cowboy_resource_owner:owner_id(Req),
      URL = cowboy_base:resolve([<<"carts">>, UserID], Req),
      Req2 = cowboy_req:set_resp_header(<<"content-location">>, URL, Req),
      flokk_cart_read:body(UserID, Cart, Req2, State);
    _ ->
      {error, 500, Req}
  end.

remove(ID, Body, Req, State) ->
  Offer = fast_key:get(<<"offer">>, Body),

  case flokk_cart:remove(ID, Offer) of
    {ok, Cart} ->
      UserID = cowboy_resource_owner:owner_id(Req),
      URL = cowboy_base:resolve([<<"carts">>, UserID], Req),
      Req2 = cowboy_req:set_resp_header(<<"content-location">>, URL, Req),
      flokk_cart_read:body(UserID, Cart, Req2, State);
    _ ->
      {error, 500, Req}
  end.

%% Steps:
%%   validate the address and cc
%%   sum the cart total (+ tax and shipping)
%%   charge the card through balanced
%%   create a purchase resource
%%   ** async **
%%   create a PO
%%   send a receipt email to the customer
%%   notify us that we got a sale
checkout(ID, Body, Req, State) ->
  CreditCard = fast_key:get(<<"creditCard">>, Body),

  Total = 1000,
  Description = <<"The Flokk LLC">>, %% TODO make a payment description

  {success, _} = balanced:credit_from_uri(CreditCard, Total, Description),

  flokk_cart:clear(ID),
  {ok, Req, State}.
