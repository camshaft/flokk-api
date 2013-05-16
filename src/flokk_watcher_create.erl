-module (flokk_watcher_create).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, _) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  Req1 = cowboy_req:set_meta(body, [], Req),
  {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
  ok.
