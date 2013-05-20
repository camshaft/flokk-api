-module(flokk_item_create).

-export([init/2]).
-export([scope/2]).
-export([validate/3]).
-export([create/3]).
-export([location/3]).

-define(SCOPE, <<"item.create">>).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

%% TODO validate body
validate(_Body, Req, State) ->
  {true, Req, State}.

create(Body, Req, State) ->
  {ok, ID} = flokk_item:create(Body),
  {ID, Req, State}.

location(ID, Req, State) ->
  {flokk_util:resolve([<<"items">>, ID], Req), Req, State}.
