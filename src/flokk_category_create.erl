-module(flokk_category_create).

-export([init/2]).
-export([scope/2]).
-export([validate/3]).
-export([create/3]).
-export([location/3]).

-define(SCOPE, <<"category.create">>).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

%% TODO validate body
validate(_Body, Req, State) ->
  {true, Req, State}.

create(Body, Req, State) ->
  {ok, ID} = flokk_category:create(Body),
  {ID, Req, State}.

location(ID, Req, State) ->
  {flokk_util:resolve([<<"categories">>, ID], Req), Req, State}.
