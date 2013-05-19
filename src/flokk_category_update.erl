-module(flokk_category_update).

-export([scope/2]).
-export([validate/3]).
-export([update/4]).
-export([body/4]).

-define(SCOPE, <<"category.update">>).

scope(Req, State) ->
  {?SCOPE, Req, State}.

%% TODO validate body
validate(_Body, Req, State) ->
  {true, Req, State}.

update(ID, Body, Req, State) ->
  case flokk_category:update(ID, Body) of
    {error, notfound} -> {error, 404, Req};
    {error, _} -> {error, 500, Req};
    {ok, Category} -> {Category, Req, State}
  end.

body(ID, Category, Req, State) ->
  flokk_category_read:body(ID, Category, Req, State).
