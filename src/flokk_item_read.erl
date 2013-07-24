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
  URL = cowboy_base:resolve([<<"items">>, ID], Req),
  Name = fast_key:get(<<"name">>, Item, <<>>),
  Description = fast_key:get(<<"description">>, Item, <<>>),
  Category = fast_key:get(<<"category">>, Item, <<"misc">>),
  Retail = fast_key:get(<<"retail">>, Item, <<>>),
  MinPrice = fast_key:get(<<"max-discount">>, Item, 0),
  Shipping = fast_key:get(<<"shipping">>, Item, 0),
  Currency = fast_key:get(<<"currency">>, Item, <<"USD">>),
  Keywords = fast_key:get(<<"keywords">>, Item, []),
  VendorID = fast_key:get(<<"vendor_id">>, Item, <<>>),
  VendorTitle = fast_key:get(<<"vendor_title">>, Item, <<>>),
  Image = fast_key:get(<<"image">>, Item, <<>>),
  Images = fast_key:get(<<"images">>, Item, <<>>),
  _Options = fast_key:get(<<"options">>, Item, []),

  P = presenterl:create(),

  P ! [
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
    {<<"watchers">>, [
      {<<"href">>, cowboy_base:resolve([<<"items">>, ID, <<"watchers">>], Req)}
    ]},
    {<<"offers">>, [
      {<<"href">>, cowboy_base:resolve([<<"items">>, ID, <<"sale">>], Req)}
    ]},
    {<<"category">>, [
      {<<"href">>, cowboy_base:resolve([<<"categories">>, Category], Req)}
    ]},
    {<<"publisher">>, [
      {<<"href">>, cowboy_base:resolve([<<"vendors">>, VendorID], Req)},
      {<<"title">>, VendorTitle}
    ]},
    {<<"image">>, [
      {<<"src">>, Image},
      {<<"type">>, <<"image/jpeg">>} %% TODO should we store this here or just get it from the extension
    ]},
    {<<"alt-image">>, [
      [
        {<<"src">>, Src},
        {<<"type">>, <<"image/jpeg">>}
      ] || Src <- Images
    ]}
  ],

  presenterl:conditional([
    cowboy_resource_owner:is_authorized(<<"item.update">>, Req)
  ], [
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
  ], P),

  presenterl:conditional([
    cowboy_resource_owner:is_authorized(<<"item.delete">>, Req)
  ], [
    {<<"delete">>, [
      {<<"action">>, URL},
      {<<"method">>, <<"DELETE">>}
    ]}
  ], P),

  Body = presenterl:encode(P),

  {Body, Req, State}.

ttl(Req, State) ->
  {3600, Req, State}.
