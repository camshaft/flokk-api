-module(flokk_category_read).

-export([init/2]).
-export([read/3]).
-export([body/4]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

read(ID, Req, State) ->
  Response = flokk_category:read(ID, cowboy_env:get(Req)),
  {Response, Req, State}.

body(ID, Category, Req, State) ->
  URL = cowboy_base:resolve([<<"category">>, ID], Req),

  Title = fast_key:get(<<"title">>, Category, <<>>),
  Label = fast_key:get(<<"label">>, Category, <<>>),
  Promo = fast_key:get(<<"promo">>, Category, <<>>),

  P = presenterl:create(),

  P ! [
    {<<"title">>, Title},
    {<<"items">>, [
      {<<"href">>, cowboy_base:resolve([<<"categories">>, ID, <<"items">>], Req)}
    ]},
    {<<"label">>, [
      {<<"href">>, Label},
      {<<"type">>, <<"image/svg">>}
    ]},
    {<<"promo">>, [
      {<<"href">>, Promo},
      {<<"type">>, <<"image/jpg">>}
    ]}
  ],

  presenterl:conditional([
    cowboy_resource_owner:is_authorized([<<"category.update">>], Req)
  ], [
    {<<"update">>, [
      {<<"action">>, URL},
      {<<"method">>, <<"PUT">>},
      {<<"input">>, [
        {<<"title">>, [
          {<<"type">>, <<"text">>},
          {<<"value">>, Title}
        ]},
        {<<"label">>, [
          {<<"type">>, <<"url">>},
          {<<"value">>, Label}
        ]},
        {<<"promo">>, [
          {<<"type">>, <<"url">>},
          {<<"value">>, Promo}
        ]}
      ]}
    ]}
  ], P),

  presenterl:conditional([
    cowboy_resource_owner:is_authorized([<<"category.delete">>], Req)
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
