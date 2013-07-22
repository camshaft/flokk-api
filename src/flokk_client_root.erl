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
  P = presenterl:create(),

  presenterl:conditional([
    cowboy_resource_owner:is_authorized([<<"client.find">>], Req)
  ], [
    {<<"find">>, [
      {<<"action">>, cowboy_base:resolve([<<"clients">>,<<"find">>], Req)},
      {<<"method">>, <<"GET">>},
      {<<"input">>, [
        {<<"id">>, [
          {<<"type">>, <<"application/x-oauth-client-id">>}
        ]}
      ]}
    ]}
  ], P),

  presenterl:conditional([
    cowboy_resource_owner:is_authorized([<<"client.create">>], Req)
  ], [
    {<<"create">>, [
      {<<"action">>, cowboy_base:resolve([<<"clients">>], Req)},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        {<<"name">>, [
          {<<"type">>, <<"text">>}
        ]},
        {<<"description">>, [
          {<<"type">>, <<"text">>}
        ]},
        {<<"redirect_uri">>, [
          {<<"type">>, <<"url">>},
          {<<"multiple">>, true}
        ]},
        {<<"scopes">>, [
          {<<"type">>, <<"application/x-oauth-client-scope">>},
          {<<"multiple">>, true},
          {<<"options">>, [
            %% TODO populate the available scopes
          ]}
        ]}
      ]}
    ]}
  ], P),

  Body = presenterl:encode(P),

  {Body, Req, State}.

ttl(Req, State)->
  {3600, Req, State}.
