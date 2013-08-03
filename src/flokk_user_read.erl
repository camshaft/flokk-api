-module(flokk_user_read).

-export([init/2]).
-export([read/3]).
-export([body/4]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

read(ID, Req, State) ->
  Response = flokk_user:read(ID, cowboy_env:get(Req)),
  {Response, Req, State}.

body(ID, User, Req, State) ->
  IsOwner = case cowboy_resource_owner:owner_id(Req) of
    ID -> true;
    undefined -> true;
    _ -> false
  end,

  P = presenterl:create(),

  URL = cowboy_base:resolve([<<"users">>,ID], Req),

  presenterl:conditional([
    IsOwner,
    cowboy_resource_owner:is_authorized(<<"user.id">>, Req)
  ], [
    {<<"id">>, ID}
  ], P),

  [presenterl:conditional([
    IsOwner,
    cowboy_resource_owner:is_authorized([<<"user.", Prop/binary>>], Req)
  ], [
    {Prop, fast_key:get(Prop, User)}
  ], P) || Prop <- [<<"email">>, <<"birthday">>, <<"passhash">>, <<"name">>]],

  presenterl:conditional([
    IsOwner,
    cowboy_resource_owner:is_authorized(<<"cart.read">>, Req)
  ], [
    {<<"cart">>, [
      {<<"href">>, cowboy_base:resolve([<<"carts">>, ID], Req)}
    ]}
  ], P),

  presenterl:conditional([
    IsOwner,
    cowboy_resource_owner:is_authorized(<<"user.credit-card.read">>, Req)
  ], [
    {<<"creditCard">>, [
      format_credit_card(Card, URL, Req) || Card <- fast_key:get(<<"credit_cards">>, User, [])
    ]}
  ], P),

  presenterl:conditional([
    IsOwner,
    cowboy_resource_owner:is_authorized(<<"user.update">>, Req)
  ], [
    {<<"update">>, [
      {<<"action">>, URL},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        {<<"action">>, [
          {<<"type">>, <<"hidden">>},
          {<<"value">>, <<"update">>}
        ]},
        {<<"email">>, [
          {<<"type">>, <<"email">>},
          {<<"required">>, true},
          {<<"value">>, fast_key:get(<<"email">>, User)}
        ]},
        {<<"name">>, [
          {<<"type">>, <<"text">>},
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
    ]}
  ], P),

  presenterl:conditional([
    IsOwner,
    cowboy_resource_owner:is_authorized(<<"user.update">>, Req)
  ], [
    {<<"createCreditCard">>, [
      {<<"action">>, URL},
      {<<"method">>, <<"POST">>},
      {<<"profile">>, [
        {<<"href">>, <<"http://alps.io/schema.org/CreditCard.xml">>}
      ]},
      {<<"input">>, [
        {<<"action">>, [
          {<<"type">>, <<"hidden">>},
          {<<"value">>, <<"add_credit_card">>}
        ]},
        {<<"url">>, [
          {<<"type">>, <<"x-balanced-uri">>}
        ]},
        {<<"name">>, [
          {<<"type">>, <<"text">>}
        ]},
        {<<"description">>, [
          {<<"type">>, <<"text">>}
        ]},
        {<<"lastDigits">>, [
          {<<"type">>, <<"text">>}
        ]},
        {<<"expirationMonth">>, [
          {<<"type">>, <<"text">>}
        ]},
        {<<"expirationYear">>, [
          {<<"type">>, <<"text">>}
        ]},
        {<<"additionalType">>, [
          {<<"type">>, <<"text">>}
        ]}
      ]}
    ]}
  ], P),

  presenterl:conditional([
    IsOwner,
    cowboy_resource_owner:is_authorized(<<"user.delete">>, Req)
  ], [
    {<<"delete">>, [
      {<<"action">>, URL},
      {<<"method">>, <<"DELETE">>},
      {<<"input">>, []}
    ]}
  ], P),

  Body = presenterl:encode(P),

  {Body, Req, State}.

format_credit_card(Card, UserURL, Req) ->
  URL = fast_key:get(<<"url">>, Card),

  %% Don't use presenterl here because it's one conditional

  cowboy_resource_builder:authorize(<<"user.credit-card.update">>, Req, [
    {<<"profile">>, [
      {<<"href">>, <<"http://alps.io/schema.org/CreditCard.xml">>}
    ]},
    {<<"url">>, URL},
    {<<"name">>, fast_key:get(<<"name">>, Card)},
    {<<"description">>, fast_key:get(<<"description">>, Card)},
    {<<"lastDigits">>,  fast_key:get(<<"lastDigits">>, Card)},
    {<<"expirationMonth">>,  fast_key:get(<<"expirationMonth">>, Card)},
    {<<"expirationYear">>,  fast_key:get(<<"expirationYear">>, Card)},
    {<<"additionalType">>,  fast_key:get(<<"additionalType">>, Card)}
  ], [
    {<<"delete">>, [
      {<<"action">>, UserURL},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        {<<"action">>, [
          {<<"type">>, <<"hidden">>},
          {<<"value">>, <<"remove_credit_card">>}
        ]},
        {<<"url">>, [
          {<<"type">>, <<"hidden">>},
          {<<"value">>, URL}
        ]}
      ]}
    ]}
  ]).

ttl(Req, State) ->
  {3600, Req, State}.
