-module (flokk_user_read).

-export([init/2]).
-export([read/3]).
-export([body/4]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

read(ID, Req, State) ->
  case flokk_user:read(ID) of
    {error, notfound} -> {error, 404, Req};
    {error, _} -> {error, 500, Req};
    {ok, User} -> {User, Req, State}
  end.

body(ID, User, Req, State) ->
  Body = [],

  Body2 = cowboy_resource_builder:authorize([ID, undefined], <<"user.id">>, Req, Body, [
    {<<"id">>, ID}
  ]),

  Body3 = cowboy_resource_builder:authorize([ID, undefined], <<"user.email">>, Req, Body2, [
    {<<"email">>, fast_key:get(<<"email">>, User)}
  ]),

  Body4 = cowboy_resource_builder:authorize([ID, undefined], <<"user.birthday">>, Req, Body3, [
    {<<"birthday">>, fast_key:get(<<"birthday">>, User)}
  ]),

  Body5 = cowboy_resource_builder:authorize([ID, undefined], <<"user.passhash">>, Req, Body4, [
    {<<"passhash">>, fast_key:get(<<"passhash">>, User)}
  ]),

  Body6 = cowboy_resource_builder:authorize([ID, undefined], <<"user.name">>, Req, Body5, [
    {<<"name">>, fast_key:get(<<"name">>, User)}
  ]),

  Body7 = cowboy_resource_builder:authorize(ID, <<"user.credit_card">>, Req, Body6, [
    {<<"name">>, fast_key:get(<<"name">>, User)}
  ]),

  Body8 = cowboy_resource_builder:authorize(<<"cart.read">>, Req, Body7, [
    {<<"cart">>, [
      {<<"href">>, cowboy_base:resolve([<<"carts">>, ID], Req)}
    ]}
  ]),

  Body9 = cowboy_resource_builder:authorize(ID, <<"user.update">>, Req, Body8, [
    {<<"action">>, cowboy_base:resolve([<<"users">>,ID], Req)},
    {<<"method">>, <<"PUT">>},
    {<<"input">>, [
      {<<"email">>, [
        {<<"type">>, <<"application/x-flokk-email">>},
        {<<"required">>, true},
        {<<"value">>, fast_key:get(<<"email">>, User)}
      ]},
      {<<"name">>, [
        {<<"type">>, <<"application/x-flokk-name">>},
        {<<"value">>, fast_key:get(<<"name">>, User)}
      ]},
      {<<"birthday">>, [
        {<<"type">>, <<"application/x-flokk-birthday">>},
        {<<"value">>, fast_key:get(<<"birthday">>, User)}
      ]},
      {<<"gender">>, [
        {<<"type">>, <<"select">>},
        {<<"value">>, fast_key:get(<<"gender">>, User)},
        {<<"options">>, [
          [
            {<<"value">>, <<"m">>},
            {<<"prompt">>, <<"Male">>}
          ],
          [
            {<<"value">>, <<"f">>},
            {<<"prompt">>, <<"Female">>}
          ]
        ]}
      ]}
    ]}
  ]),

  {Body9, Req, State}.

ttl(Req, State) ->
  {3600, Req, State}.
