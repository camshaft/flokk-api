-module (flokk_middleware_method_match).

-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
  [Method, Path] = cowboy_req:get([method, path], Req),
  {ok, cowboy_req:set([{path, <<"/",Method/binary," ",Path/binary>>}], Req), [{orig_path, Path}|Env]}.
