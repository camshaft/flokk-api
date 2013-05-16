-module (flokk_util).

-export ([env/1]).
-export ([env/2]).
-export ([get_value/2]).
-export ([get_value/3]).
-export ([load_dispatch/1]).
-export ([resolve/2]).
-export ([resolve/3]).
-export ([resolve/4]).
-export ([resolve/5]).
-export ([resolve/6]).
-export ([resolve/7]).
-export ([resolve/8]).
-export ([resolve/9]).
-export ([resolve/10]).

env(Name) ->
  env(Name, false).
env(Name, Default)->
  case os:getenv(Name) of
    false ->
      Default;
    Value ->
      Value
  end.

get_value(Key, List) ->
  get_value(Key, List, undefined).
get_value(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    false -> Default;
    Value -> Value
  end.

load_dispatch(App) ->
  File = priv_dir(App),
  {ok, HostConfs} = file:consult(filename:join(File, "routes.econf")),
  [patch_method(Host, Paths) || {Host, Paths} <- HostConfs].

patch_method(Host, Paths) ->
  {Host, [{"/"++Method++" "++Path, Handler, Options} || {Method, Path, Handler, Options} <- Paths]}.

priv_dir(Mod) ->
  {ok, App} = application:get_application(?MODULE),
  case code:priv_dir(App) of
    {error, _} ->
      Ebin = filename:dirname(code:which(Mod)),
      filename:join(filename:dirname(Ebin), "priv");
    Path -> Path
  end.

resolve(<<"/">>, Req) ->
  {Base, Req} = cowboy_req:meta(base, Req),
  <<Base/binary, "/">>;
resolve(<<"/",Path/binary>>, Req) ->
  resolve(Path, Req);
resolve(Path, Req) ->
  {Base, Req} = cowboy_req:meta(base, Req),
  <<Base/binary, "/", Path/binary>>.

resolve(P1, P2, Req) ->
  resolve(binary_join([P1, P2], <<"/">>), Req).
resolve(P1, P2, P3, Req) ->
  resolve(binary_join([P1, P2, P3], <<"/">>), Req).
resolve(P1, P2, P3, P4, Req) ->
  resolve(binary_join([P1, P2, P3, P4], <<"/">>), Req).
resolve(P1, P2, P3, P4, P5, Req) ->
  resolve(binary_join([P1, P2, P3, P4, P5], <<"/">>), Req).
resolve(P1, P2, P3, P4, P5, P6, Req) ->
  resolve(binary_join([P1, P2, P3, P4, P5, P6], <<"/">>), Req).
resolve(P1, P2, P3, P4, P5, P6, P7, Req) ->
  resolve(binary_join([P1, P2, P3, P4, P5, P6, P7], <<"/">>), Req).
resolve(P1, P2, P3, P4, P5, P6, P7, P8, Req) ->
  resolve(binary_join([P1, P2, P3, P4, P5, P6, P7, P8], <<"/">>), Req).
resolve(P1, P2, P3, P4, P5, P6, P7, P8, P9, Req) ->
  resolve(binary_join([P1, P2, P3, P4, P5, P6, P7, P8, P9], <<"/">>), Req).

binary_join([], _Sep) ->
  <<>>;
binary_join([H], _Sep) ->
  << H/binary >>;
binary_join([H | T], Sep) ->
  << H/binary, Sep/binary, (binary_join(T, Sep))/binary >>.
