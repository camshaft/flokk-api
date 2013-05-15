-module (flokk_middleware_base).

-export([execute/2]).

execute(Req, Env) ->
  Proto = choose(cowboy_req:header(<<"x-forwarded-proto">>, Req), {<<"http">>, Req}),
  Host = choose(cowboy_req:header(<<"x-forwarded-host">>, Req), cowboy_req:host(Req)),
  Port = choose(cowboy_req:header(<<"x-forwarded-port">>, Req), cowboy_req:port(Req)),
  Path = choose(cowboy_req:header(<<"x-forwarded-path">>, Req), {<<"">>, Req}),
  Req2 = cowboy_req:set_meta(base, format(Proto, Host, Port, Path), Req),
  {ok, Req2, Env}.

choose({undefined, _}, {B, _}) -> B;
choose({A, _}, _) -> A.

format(<<"http">>, Host, 80, Path) ->
  <<"http://",Host/binary,Path/binary>>;
format(<<"https">>, Host, 443, Path) ->
  <<"https://",Host/binary,Path/binary>>;
format(Proto, Host, Port, Path) ->
  StringPort = list_to_binary(integer_to_list(Port)),
  <<Proto/binary,"://",Host/binary,":",StringPort/binary,Path/binary>>.
