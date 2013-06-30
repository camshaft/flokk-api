-module(flokk_client_find).

-export([init/2]).
-export([list/2]).
-export([scope/2]).
-export([body/3]).
-export([ttl/2]).

-define(SCOPE, <<"client.find">>).

init(Req, _Opts) ->
  {ok, Req, []}.

list(Req, State) ->
  {ID, Req2} = cowboy_req:qs_val(<<"id">>, Req),

  Query = [
    {<<"id">>, ID}
  ],

  case flokk_client:find(Query) of
    {error, _} ->
      {error, 500, Req2};
    {ok, Results} ->
      {Results, Req2, State}
  end.

scope(Req, State) ->
  {?SCOPE, Req, State}.

body(Results, Req, State) ->
  Body = [
    {<<"results">>, [format_client(ClientID, Req) || ClientID <- Results]}
  ],

  {Body, Req, State}.

format_client(ClientID, Req)->
  [
    {<<"href">>, cowboy_base:resolve([<<"clients">>, ClientID], Req)}
  ].

ttl(Req, State)->
  {3600, Req, State}.
