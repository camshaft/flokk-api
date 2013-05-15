-module (flokk_util).

-export ([env/1]).
-export ([env/2]).
-export ([get_value/2]).
-export ([get_value/3]).
-export ([resolve/2]).

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

resolve(Path, Req) ->
  {Base, Req} = cowboy_req:meta(base, Req),
  <<Base/binary, Path/binary>>.
