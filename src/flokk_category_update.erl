-module (flokk_category_update).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  Req1 = cowboy_req:set_meta(body, [], Req),
  {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
  ok.
