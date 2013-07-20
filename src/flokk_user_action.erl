-module(flokk_user_action).

-export([init/2]).
-export([scope/2]).
-export([validate/3]).
-export([update/4]).
-export([add_credit_card/4]).
-export([remove_credit_card/4]).

-define(SCOPE, <<"user.update">>).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

validate(_Body, Req, State) ->
  {true, Req, State}.

update(_ID, _Body, Req, State) ->
  {ok, Req, State}.

add_credit_card(UserID, CreditCard, Req, State) ->
  case flokk_user:add_credit_card(UserID, CreditCard) of
    {ok, User} ->
      URL = cowboy_base:resolve([<<"users">>, UserID], Req),
      Req2 = cowboy_req:set_resp_header(<<"content-location">>, URL, Req),
      flokk_user_read:body(UserID, User, Req2, State);
    _ ->
      {error, Req, State}
  end.

remove_credit_card(_ID, _Body, Req, State) ->
  {ok, Req, State}.
