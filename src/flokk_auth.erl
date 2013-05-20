-module(flokk_auth).

-export([execute/2]).
-export([build/4]).
-export([authorize/2]).
-export([logged_in/1]).
-export([user_id/1]).

execute(Req, Env) ->
  lager:debug("middleware:auth"),
  Req2 = cowboy_req:set_meta(token_secret, proplists:get_value(token_secret, Env), Req),
  Req3 = cowboy_req:set_meta(scopes_enum, proplists:get_value(scopes_enum, Env, []), Req2),
  {ok, Req3, Env}.


build(Scope, Req, Body, Condition)->
  case authorize(Scope, Req) of
    false -> Body;
    _ -> lists:concat([Body,Condition])
  end.

authorize(Scope, Req)->
  {Scopes, Req} = cowboy_req:meta(scopes, Req, []),
  {Enum, Req} = cowboy_req:meta(scopes_enum, Req, []),
  case proplists:get_value(Scope, Enum) of
    undefined -> false;
    Short -> lists:member(Short, Scopes)
  end.

logged_in(Req)->
  UserId = user_id(Req),
  UserId =/= undefined.

user_id(Req) ->
  {UserId, Req} = cowboy_req:meta(user_id, Req),
  format_user_id(UserId).

format_user_id(UserId) when is_binary(UserId) ->
  UserId;
format_user_id(UserId) when is_integer(UserId) ->
  list_to_binary(integer_to_list(UserId));
format_user_id(_) ->
  undefined.
