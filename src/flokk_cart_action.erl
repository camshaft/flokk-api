-module(flokk_cart_action).

-export([init/2]).
-export([scope/2]).
-export([validate/3]).
-export([action/4]).

-define(SCOPE, [<<"cart.update">>]).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

%% TODO validate body
validate(_Body, Req, State) ->
  {true, Req, State}.

action(ID, Body, Req, State) ->
  Offer = fast_key:get(<<"offer">>, Body),
  Result = case fast_key:get(<<"quantity">>, Body) of
    0 ->
      flokk_cart:remove(ID, Offer);
    Quantity ->
      flokk_cart:set(ID, Offer, Quantity)
  end,

  case Result of
    {ok, Cart} ->
      {CartBody, Req2, State2} = flokk_cart_read:body(ID, Cart, Req, State),
      JSON = jsx:encode(CartBody),
      URL = cowboy_base:resolve([<<"carts">>, ID], Req2),
      Req3 = cowboy_req:set_resp_header(<<"location">>, URL, Req2),
      Req4 = cowboy_req:set_resp_header(<<"content-location">>, URL, Req3),
      Req5 = cowboy_req:set_resp_body(JSON, Req4),
      {true, Req5, State2};
    _ ->
      io:format("~p~n", [Result]),
      {error, 500, Req}
  end.
