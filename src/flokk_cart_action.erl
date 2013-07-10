-module(flokk_cart_action).

-export([init/2]).
-export([scope/2]).
-export([validate/3]).
-export([action/4]).

-define(SCOPE, [<<"cart.add">>, <<"cart.remove">>]).

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
    ok ->
      Req2 = cowboy_req:set_resp_body(<<"{}">>, Req),
      {true, Req2, State};
    _ ->
      {error, 500, Req}
  end.
