-module(flokk_category_delete).

-export([scope/2]).
-export([delete/3]).
-export([location/2]).

-define (SCOPE, <<"category.delete">>).

scope(Req, State) ->
  {?SCOPE, Req, State}.

delete(ID, Req, State) ->
  ok = flokk_category:delete(ID),
  {ok, Req, State}.

location(Req, State) ->
  {flokk_util:resolve(<<"categories">>, Req), Req, State}.
