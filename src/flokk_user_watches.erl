-module(flokk_user_watches).

-export([init/2]).
-export([scope/2]).
-export([validate/3]).
-export([watch/4]).

-define(SCOPE, <<"user.watches">>).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

validate(_Body, Req, State) ->
  {true, Req, State}.

watch(_UserID, _Body, Req, State) ->
  {ok, Req, State}.
