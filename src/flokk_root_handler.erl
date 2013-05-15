-module (flokk_root_handler).

-export([init/3]).
-export([resource_exists/2]).
-export([content_types_provided/2]).
-export([to_json/2]).

init(_Transport, _Req, _State) ->
  {upgrade, protocol, cowboy_rest}.

resource_exists(Req, State) ->
  {User, Req} = cowboy_req:meta(user, Req),
  CartID = flokk_util:get_value(<<"cart">>, User, <<"__CART_ID__">>),
  Body = [
    {<<"href">>, flokk_util:resolve(<<"/">>, Req)},
    {<<"sales">>, [
      {<<"href">>, flokk_util:resolve(<<"/categories/sales">>, Req)},
      {<<"title">>, <<"Sales">>}
    ]},
    {<<"cart">>, [
      {<<"href">>, flokk_util:resolve(<<"/cart/",CartID/binary>>, Req)},
      {<<"title">>, <<"Cart">>}
    ]},
    {<<"categories">>, [
      {<<"href">>, flokk_util:resolve(<<"/categories">>, Req)},
      {<<"title">>, <<"Categories">>}
    ]}
  ],
  Body2 = case flokk_util:get_value(<<"id">>, User) of
    undefined -> Body;
    UserID -> [{<<"profile">>, [{<<"href">>, flokk_util:resolve(<<"/users/",UserID/binary>>, Req)}]}|Body]
  end,
  {true, Req, [{body, Body2}|State]}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, to_json}
  ], Req, State}.

to_json(Req, State) ->
  Body = proplists:get_value(body, State),
  {jsx:encode(Body), Req, State}.

%% TODO set a really long expiration header
