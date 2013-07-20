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
  URL = cowboy_base:resolve([<<"users">>,ID], Req),

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

  Body7 = cowboy_resource_builder:authorize(ID, <<"user.credit-card.read">>, Req, Body6, [
    {<<"creditCard">>, [
      format_credit_card(Card, URL, Req) || Card <- fast_key:get(<<"credit_cards">>, User, [])
    ]}
  ]),

  Body8 = cowboy_resource_builder:authorize(<<"cart.read">>, Req, Body7, [
    {<<"cart">>, [
      {<<"href">>, cowboy_base:resolve([<<"carts">>, ID], Req)}
    ]}
  ]),

  Body9 = cowboy_resource_builder:authorize(ID, <<"user.update">>, Req, Body8, [
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
  ]),

  Body10 = cowboy_resource_builder:authorize(ID, <<"user.credit-card.update">>, Req, Body9, [
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
  ]),

  {Body10, Req, State}.

format_credit_card(Card, UserURL, Req) ->
  URL = fast_key:get(<<"url">>, Card),
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
