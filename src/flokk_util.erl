-module (flokk_util).

-export ([env/1]).
-export ([env/2]).
-export ([get_value/2]).
-export ([get_value/3]).
-export ([load_dispatch/1]).
-export ([resolve/2]).
-export ([binary_join/2]).

env(Name) ->
  env(Name, undefined).
env(Name, Default)->
  case os:getenv(Name) of
    false -> Default;
    Value -> Value
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
  HostConfs.

priv_dir(Mod) ->
  {ok, App} = application:get_application(?MODULE),
  case code:priv_dir(App) of
    {error, _} ->
      Ebin = filename:dirname(code:which(Mod)),
      filename:join(filename:dirname(Ebin), "priv");
    Path -> Path
  end.

resolve(Parts, Req) when is_list(Parts) ->
  resolve(binary_join(Parts, <<"/">>), Req);
resolve(<<"/">>, Req) ->
  {Base, Req} = cowboy_req:meta(base, Req),
  <<Base/binary, "/">>;
resolve(<<"/",Path/binary>>, Req) ->
  resolve(Path, Req);
resolve(Path, Req) ->
  {Base, Req} = cowboy_req:meta(base, Req),
  <<Base/binary, "/", Path/binary>>.

binary_join([], _Sep) ->
  <<>>;
binary_join([H], _Sep) ->
  << H/binary >>;
binary_join([H | T], Sep) ->
  << H/binary, Sep/binary, (binary_join(T, Sep))/binary >>.
