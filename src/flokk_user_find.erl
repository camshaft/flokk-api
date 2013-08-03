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
  Result = exec(ID, Email, Facebook, Google, cowboy_env:get(Req4)),
  {Result, Req4, State}.

exec(ID, _, _, _, Env) when is_binary(ID) ->
  case flokk_user:read(ID, Env) of
    {ok, _User} ->
      {ok, [ID]};
    {error, notfound} ->
      {ok, []};
    Error ->
      Error
  end;
exec(_, Email, _, _, Env) when is_binary(Email) ->
  flokk_user:find(<<"email">>, Email, Env);
exec(_, _, Facebook, _, Env) when is_binary(Facebook) ->
  flokk_user:find(<<"facebook">>, Facebook, Env).

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
