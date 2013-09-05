-module(flokk_vendor_read).

-export([init/2]).
-export([read/3]).
-export([body/4]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

read(ID, Req, State) ->
  Response = flokk_vendor:read(ID, cowboy_env:get(Req)),
  {Response, Req, State}.

body(ID, Vendor, Req, State) ->
  URL = cowboy_base:resolve([<<"vendors">>, ID], Req),
  Name = fast_key:get(<<"name">>, Vendor, <<>>),
  Description = fast_key:get(<<"description">>, Vendor, <<>>),
  Email = fast_key:get(<<"email">>, Vendor, <<>>),
  Location = fast_key:get(<<"location">>, Vendor, <<>>),
  Logo = fast_key:get(<<"logo">>, Vendor, <<>>),

  Body = [
    {<<"profile">>, [
      {<<"href">>, <<"http://alps.io/schema.org/Organization.xml">>}
    ]},
    {<<"name">>, Name},
    {<<"description">>, Description},
    {<<"email">>, Email},
    {<<"location">>, Location},
    {<<"logo">>, [
      {<<"src">>, Logo},
      {<<"type">>, <<"image/jpg">>} %% TODO get file extension
    ]},
    {<<"makesOffer">>, [
      {<<"href">>, cowboy_base:resolve([<<"vendors">>, ID, <<"items">>], Req)}
    ]}
  ],

  %% TODO also verify that they have access to this vendor profile
  Body1 = cowboy_resource_builder:authorize(<<"vendor.update">>, Req, Body, [
    {<<"update">>, [
      {<<"action">>, URL},
      {<<"method">>, <<"PUT">>},
      {<<"input">>, [
        {<<"name">>, [
          {<<"type">>, <<"text">>},
          {<<"value">>, Name},
          {<<"required">>, true}
        ]},
        {<<"description">>, [
          {<<"type">>, <<"text">>},
          {<<"value">>, Description}
        ]},
        {<<"email">>, [
          {<<"type">>, <<"email">>},
          {<<"value">>, Email}
        ]},
        {<<"location">>, [
          {<<"type">>, <<"text">>},
          {<<"location">>, Location}
        ]}
      ]}
    ]}
  ]),

  %% TODO also verify that they have access to this vendor profile
  Body2 = cowboy_resource_builder:authorize(<<"vendor.delete">>, Req, Body1, [
    {<<"delete">>, [
      {<<"action">>, URL},
      {<<"method">>, <<"DELETE">>}
    ]}
  ]),

  {Body2, Req, State}.

ttl(Req, State) ->
  {3600, Req, State}.
