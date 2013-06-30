-module(flokk_user_root).

-export([init/2]).
-export([scope/2]).
-export([body/2]).
-export([ttl/2]).

-define(SCOPE, <<"user">>).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

body(Req, State) ->
  Body = [],

  Body2 = cowboy_resource_builder:authorize(<<"user.find">>, Req, Body, [
    {<<"find">>, [
      {<<"action">>, cowboy_base:resolve([<<"users">>,<<"find">>], Req)},
      {<<"method">>, <<"GET">>},
      {<<"input">>, [
        {<<"id">>, [
          {<<"type">>, <<"application/x-flokk-user-id">>}
        ]},
        {<<"email">>, [
          {<<"type">>, <<"application/x-flokk-email">>}
        ]},
        {<<"facebook">>, [
          {<<"type">>, <<"application/x-facebook-user-id">>}
        ]},
        {<<"google">>, [
          {<<"type">>, <<"application/x-google-user-id">>}
        ]}
      ]}
    ]}
  ]),

  Body3 = cowboy_resource_builder:authorize(<<"user.create">>, Req, Body2, [
    {<<"create">>, [
      {<<"action">>, cowboy_base:resolve([<<"users">>], Req)},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        {<<"email">>, [
          {<<"type">>, <<"application/x-flokk-email">>},
          {<<"required">>, true}
        ]},
        {<<"passhash">>, [
          {<<"type">>, <<"application/x-flokk-passhash">>},
          {<<"required">>, true}
        ]},
        {<<"name">>, [
          {<<"type">>, <<"application/x-flokk-name">>}
        ]},
        {<<"birthday">>, [
          {<<"type">>, <<"application/x-flokk-birthday">>}
        ]},
        {<<"locale">>, [
          {<<"type">>, <<"application/x-flokk-locale">>}
        ]},
        {<<"gender">>, [
          {<<"type">>, <<"application/x-flokk-gender">>}
        ]},
        {<<"facebook">>, [
          {<<"type">>, <<"application/x-facebook-user-id">>}
        ]},
        {<<"google">>, [
          {<<"type">>, <<"application/x-google-user-id">>}
        ]}
      ]}
    ]}
  ]),

  {Body3, Req, State}.

ttl(Req, State)->
  {3600, Req, State}.
