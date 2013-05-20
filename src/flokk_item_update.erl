-module(flokk_item_update).

-export([init/2]).
-export([scope/2]).
-export([validate/3]).
-export([update/4]).

-define(SCOPE, <<"item.update">>).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

%% TODO validate body
validate(_Body, Req, State) ->
  {true, Req, State}.

update(ID, Body, Req, State) ->
  case flokk_item:update(ID, Body) of
    {error, _} -> {error, 500, Req};
    ok -> {true, Req, State}
  end.
