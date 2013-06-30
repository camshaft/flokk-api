-module(flokk_user_find).

-export([init/2]).
-export([list/2]).
-export([scope/2]).
-export([body/3]).
-export([ttl/2]).

-define(SCOPE, <<"user.find">>).

init(Req, _Opts) ->
  {ok, Req, []}.

list(Req, State) ->
  {ID, Req2} = cowboy_req:qs_val(<<"id">>, Req),
  {Email, Req2} = cowboy_req:qs_val(<<"email">>, Req),
  {Facebook, Req3} = cowboy_req:qs_val(<<"facebook">>, Req2),
  {Google, Req4} = cowboy_req:qs_val(<<"google">>, Req3),

  Query = [
    {<<"id">>, ID},
    {<<"email">>, Email},
    {<<"facebook">>, Facebook},
    {<<"google">>, Google}
  ],

  case flokk_user:find(Query) of
    {error, _} ->
      {error, 500, Req4};
    {ok, Results} ->
      {Results, Req4, State}
  end.

scope(Req, State) ->
  {?SCOPE, Req, State}.

body(Results, Req, State) ->
  Body = [
    {<<"results">>, [format_user(UserID, Req) || UserID <- Results]}
  ],

  {Body, Req, State}.

format_user(UserID, Req)->
  [
    {<<"href">>, cowboy_base:resolve([<<"users">>, UserID], Req)}
  ].

ttl(Req, State)->
  {3600, Req, State}.
