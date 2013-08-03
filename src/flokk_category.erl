-module(flokk_category).

%% API.
-export([list/1]).
-export([read/2]).
-export([create/2]).
-export([update/3]).
-export([delete/2]).
-export([validate/1]).

-include("flokk.hrl").

%% Bucket name.
-define(BUCKET(Env), <<"fk_category_", Env/binary>>).

%% API.

list(Env) ->
  ?FLOKK_DB:values(?BUCKET(Env)).

read(ID, Env) ->
  ?FLOKK_DB:get(?BUCKET(Env), ID).

create(Category, Env) ->
  ?FLOKK_DB:create(?BUCKET(Env), Category).

update(ID, Category, Env) ->
  ?FLOKK_DB:update(?BUCKET(Env), ID, Category).

delete(ID, Env) ->
  %% TODO what should we do about the lost items?
  ?FLOKK_DB:delete(?BUCKET(Env), ID).

validate(_Category) ->
  true.
