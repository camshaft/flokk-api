-module(flokk_client_root).

-export([init/2]).
-export([scope/2]).
-export([body/2]).
-export([ttl/2]).

-define(SCOPE, <<"client">>).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

body(Req, State) ->
  Body = [],

  Body2 = cowboy_resource_builder:authorize(<<"client.find">>, Req, Body, [
    {<<"find">>, [
      {<<"action">>, cowboy_base:resolve([<<"clients">>,<<"find">>], Req)},
      {<<"method">>, <<"GET">>},
      {<<"input">>, [
        {<<"id">>, [
          {<<"type">>, <<"application/x-oauth-client-id">>}
        ]}
      ]}
    ]}
  ]),

  Body3 = cowboy_resource_builder:authorize(<<"client.create">>, Req, Body2, [
    {<<"create">>, [
      {<<"action">>, cowboy_base:resolve([<<"clients">>], Req)},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        %% TODO figure out these fields
      ]}
    ]}
  ]),

  {Body3, Req, State}.

ttl(Req, State)->
  {3600, Req, State}.
