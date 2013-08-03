-module(cowboy_cors).

-export([execute/2]).

execute(Req, Env) ->
  case fast_key:get(cors, Env) of
    undefined ->
      {ok, Req, Env};
    Conf ->
      Req2 = apply_headers([
        {origin, <<"access-control-allow-origin">>, <<"*">>},
        {headers, <<"access-control-allow-headers">>, <<"origin, x-requested-with, authorization, content-type, cache-control">>},
        {method, <<"access-control-allow-method">>, <<"GET, POST, PUT, DELETE, HEAD">>},
        {method, <<"access-control-max-age">>, <<"31556926">>}
      ], Conf, Req),
      case cowboy_req:method(Req2) of
        {<<"OPTIONS">>, Req3} ->
          {halt, Req3};
        {_, Req3} ->
          {ok, Req3, Env}
      end
  end.

apply_headers([], _, Req) ->
  Req;
apply_headers([{Prop, Header, Default}|Headers], Conf, Req) ->
  Value = fast_key:get(Prop, Conf, Default),
  Req2 = cowboy_req:set_resp_header(Header, Value, Req),
  apply_headers(Headers, Conf, Req2).
