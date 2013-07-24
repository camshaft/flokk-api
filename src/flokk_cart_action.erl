-module(flokk_cart_action).

-export([init/2]).
-export([scope/2]).
-export([validate/3]).
-export([add/4]).
-export([remove/4]).

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
  Quantity = fast_key:get(<<"quantity">>, Body),

  case flokk_cart:add(ID, Offer, Quantity) of
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
