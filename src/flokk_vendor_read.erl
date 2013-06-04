-module (flokk_vendor_read).

-export([init/2]).
-export([read/3]).
-export([body/4]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

read(ID, Req, State) ->
  case flokk_vendor:read(ID) of
    {error, notfound} -> {error, 404, Req};
    {error, _} -> {error, 500, Req};
    {ok, Vendor} -> {Vendor, Req, State}
  end.

body(ID, Vendor, Req, State) ->
  URL = flokk_util:resolve([<<"vendor">>,ID], Req),
  Name = proplists:get_value(<<"name">>, Vendor, <<>>),
  Description = proplists:get_value(<<"description">>, Vendor, <<>>),
  Email = proplists:get_value(<<"email">>, Vendor, <<>>),
  Location = proplists:get_value(<<"location">>, Vendor, <<>>),
  Logo = proplists:get_value(<<"logo">>, Vendor, <<>>),

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
      {<<"href">>, flokk_util:resolve([<<"vendors">>,ID,<<"items">>], Req)}
    ]}
  ],

  %% TODO also verify that they have access to this vendor profile
  Body1 = flokk_auth:build(<<"vendor.update">>, Req, Body, [
    {<<"action">>, URL},
    {<<"method">>, <<"POST">>},
    {<<"input">>, [
      {<<"name">>, [
        {<<"type">>, <<"text">>},
        {<<"value">>, Name}
      ]}
    ]}
  ]),

  %% TODO also verify that they have access to this vendor profile
  Body2 = flokk_auth:build(<<"vendor.delete">>, Req, Body1, [
    {<<"action">>, URL},
    {<<"method">>, <<"DELETE">>}
  ]),

  {Body2, Req, State}.

ttl(Req, State) ->
  {3600, Req, State}.
