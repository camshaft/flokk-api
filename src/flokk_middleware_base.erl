-module (flokk_middleware_base).

-export([execute/2]).

execute(Req, Env) ->
  {Port, Req} = cowboy_req:port(Req),
  PortBin = list_to_binary(integer_to_list(Port)),
  ForwardedProto = choose(cowboy_req:header(<<"x-forwarded-proto">>, Req), {<<"http">>, Req}),
  ForwardedHost = choose(cowboy_req:header(<<"x-forwarded-host">>, Req), cowboy_req:host(Req)),
  ForwardedPort = choose(cowboy_req:header(<<"x-forwarded-port">>, Req), {PortBin, Req}),
  ForwardedPath = choose(cowboy_req:header(<<"x-forwarded-path">>, Req), {<<"">>, Req}),
  Req2 = cowboy_req:set_meta(base, format(ForwardedProto, ForwardedHost, ForwardedPort, ForwardedPath), Req),
  {ok, Req2, Env}.

choose({undefined, _}, {B, _}) -> B;
choose({A, _}, _) -> A.

format(Proto, Host, Port, <<"/">>) ->
  format(Proto, Host, Port, <<>>);
format(<<"http">>, Host, <<"80">>, Path) ->
  <<"http://",Host/binary,Path/binary>>;
format(<<"https">>, Host, <<"443">>, Path) ->
  <<"https://",Host/binary,Path/binary>>;
format(Proto, Host, Port, Path) ->
  <<Proto/binary,"://",Host/binary,":",Port/binary,Path/binary>>.
