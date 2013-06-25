-module(flokk_auth).

-export([build/4]).

build(Scope, Req, Body, Condition)->
  case cowboy_resource_owner:is_authorized(Scope, Req) of
    false -> Body;
    _ -> lists:concat([Body,Condition])
  end.
