-module (flokk_item_read).

-export([init/2]).
-export([read/3]).
-export([body/4]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

read(ID, Req, State) ->
  case flokk_item:read(ID) of
    {error, notfound} -> {error, 404, Req};
    {error, _} -> {error, 500, Req};
    {ok, Item} -> {Item, Req, State}
  end.

body(ID, Item, Req, State) ->
  URL = flokk_util:resolve([<<"items">>,ID], Req),
  Title = proplists:get_value(<<"title">>, Item, <<>>),
  Description = proplists:get_value(<<"description">>, Item, <<>>),
  Category = proplists:get_value(<<"category">>, Item, <<"1">>),
  Retail = proplists:get_value(<<"retail">>, Item, <<>>),
  MinPrice = proplists:get_value(<<"min-price">>, Item, 0),
  Shipping = proplists:get_value(<<"shipping">>, Item, 0),
  Currency = proplists:get_value(<<"currency">>, Item, <<"USD">>),
  VendorID = proplists:get_value(<<"vendor_id">>, Item, <<>>),
  VendorTitle = proplists:get_value(<<"vendor_title">>, Item, <<>>),
  Thumbnail = proplists:get_value(<<"thumbnail">>, Item, <<>>),
  Images = proplists:get_value(<<"images">>, Item, []),
  _Options = proplists:get_value(<<"options">>, Item, []),

  Body = [
    {<<"title">>, Title},
    {<<"description">>, Description},
    {<<"retail">>, Retail},
    {<<"shipping">>, Shipping},
    {<<"currency">>, Currency},
    {<<"category">>, [
      {<<"href">>, flokk_util:resolve([<<"categories">>,Category], Req)}
    ]},
    {<<"vendor">>, [
      {<<"href">>, flokk_util:resolve([<<"vendors">>,VendorID], Req)},
      {<<"title">>, VendorTitle}
    ]},
    {<<"thumbnail">>, [
      {<<"src">>, Thumbnail},
      {<<"type">>, <<"image/jpeg">>} %% TODO should we store this here or just get it from the extension
    ]},
    {<<"images">>, [
      [
        {<<"src">>, Image},
        {<<"type">>, <<"image/jpeg">>}
      ] || Image <- Images
    ]}
  ],

  Body1 = flokk_auth:build(<<"item.update">>, Req, Body, [
    {<<"update">>, [
      {<<"action">>, URL},
      {<<"method">>, <<"PUT">>},
      {<<"input">>, [
        {<<"title">>, [
          {<<"type">>, <<"text">>},
          {<<"value">>, Title}
        ]},
        {<<"description">>, [
          {<<"type">>, <<"text">>},
          {<<"value">>, Description}
        ]},
        {<<"retail">>, [
          {<<"type">>, <<"currency">>},
          {<<"value">>, Retail}
        ]},
        {<<"min-price">>, [
          {<<"type">>, <<"currency">>},
          {<<"value">>, MinPrice}
        ]},
        {<<"shipping">>, [
          {<<"type">>, <<"currency">>},
          {<<"value">>, Shipping}
        ]},
        {<<"currency">>, [
          {<<"type">>, <<"select">>},
          {<<"value">>, Currency},
          {<<"options">>, [
            [{<<"value">>, <<"USD">>}]
          ]}
        ]},
        {<<"images">>, [
          {<<"type">>, <<"url">>},
          {<<"value">>, Images}
        ]},
        {<<"thumbnail">>, [
          {<<"type">>, <<"select">>},
          {<<"value">>, Thumbnail},
          {<<"options">>, Images}
        ]}
        %% TODO have options creation form
        % {<<"options">>, [
        %   {<<"type">>, <<"">>}
        % ]}
      ]}
    ]}
  ]),

  Body2 = flokk_auth:build(<<"item.delete">>, Req, Body1, [
    {<<"action">>, URL},
    {<<"method">>, <<"DELETE">>}
  ]),

  {Body2, Req, State}.

ttl(Req, State) ->
  {3600, Req, State}.
