-module (flokk_middleware_auth).

-export([execute/2]).

execute(Req, Env) ->
  {Authorization, Req} = cowboy_req:header(<<"authorization">>, Req),
  AccessToken = access_token(Authorization),
  Req1 = cowboy_req:set_meta(access_token, AccessToken, Req),
  %% TODO decrypt the access token info and get the user scopes
  %% User = simple_secrets:unpack(AccessToken, MasterKey),
  User = [],
  Req2 = cowboy_req:set_meta(user, User, Req1),
  {ok, Req2, Env}.

access_token(<<"Bearer ",AccessToken/binary>>) ->
  AccessToken;
access_token(<<"bearer ",AccessToken/binary>>) ->
  AccessToken;
access_token(_) ->
  undefined.
