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
  Name = proplists:get_value(<<"name">>, Item, <<>>),
  Description = proplists:get_value(<<"description">>, Item, <<>>),
  Category = proplists:get_value(<<"category">>, Item, <<"1">>),
  Retail = proplists:get_value(<<"retail">>, Item, <<>>),
  MinPrice = proplists:get_value(<<"min-price">>, Item, 0),
  Shipping = proplists:get_value(<<"shipping">>, Item, 0),
  Currency = proplists:get_value(<<"currency">>, Item, <<"USD">>),
  Keywords = proplists:get_value(<<"keywords">>, Item, []),
  VendorID = proplists:get_value(<<"vendor_id">>, Item, <<>>),
  VendorTitle = proplists:get_value(<<"vendor_title">>, Item, <<>>),
  Image = proplists:get_value(<<"image">>, Item, <<>>),
  Thumbnail = proplists:get_value(<<"thumbnail">>, Item, <<>>),
  _Options = proplists:get_value(<<"options">>, Item, []),

  Body = [
    {<<"profile">>, [
      {<<"href">>, <<"http://alps.io/schema.org/ItemPage.xml">>}
    ]},
    {<<"name">>, Name},
    {<<"description">>, Description},
    {<<"retail">>, Retail},
    {<<"shipping">>, Shipping},
    {<<"currency">>, Currency},
    {<<"sku">>, ID},
    {<<"keywords">>, Keywords},
    {<<"offers">>, [
      {<<"href">>, flokk_util:resolve([<<"items">>,ID,<<"sale">>], Req)}
    ]},
    {<<"category">>, [
      {<<"href">>, flokk_util:resolve([<<"categories">>,Category], Req)}
    ]},
    {<<"publisher">>, [
      {<<"href">>, flokk_util:resolve([<<"vendors">>,VendorID], Req)},
      {<<"title">>, VendorTitle}
    ]},
    {<<"image">>, [
      {<<"src">>, Image},
      {<<"type">>, <<"image/jpeg">>} %% TODO should we store this here or just get it from the extension
    ]},
    {<<"thumbnail">>, [
      {<<"src">>, Thumbnail},
      {<<"type">>, <<"image/jpeg">>}
    ]}
  ],

  Body1 = flokk_auth:build(<<"item.update">>, Req, Body, [
    {<<"update">>, [
      {<<"action">>, URL},
      {<<"method">>, <<"PUT">>},
      {<<"input">>, [
        {<<"name">>, [
          {<<"type">>, <<"text">>},
          {<<"value">>, Name}
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
        {<<"image">>, [
          {<<"type">>, <<"url">>},
          {<<"value">>, Image}
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
