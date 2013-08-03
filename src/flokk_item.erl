-module(flokk_item).

%% API.
-export([list/1]).
-export([read/2]).
-export([create/2]).
-export([update/3]).
-export([delete/2]).
-export([find/3]).
-export([sale/2]).
-export([validate/1]).

-include("flokk.hrl").

%% Bucket name.
-define(BUCKET(Env), <<"fk_item_", Env/binary>>).

-define (TWO_I, [<<"category">>, <<"publisher">>]).

%% API.

list(Env) ->
  % TODO page this
  ?FLOKK_DB:keys(?BUCKET(Env)).

read(ID, Env) ->
  ?FLOKK_DB:get(?BUCKET(Env), ID).

create(Item, Env) ->
  % TODO validate the fields
  ?FLOKK_DB:create(?BUCKET(Env), Item, ?TWO_I).

update(ID, Item, Env) ->
  % TODO validate the fields
  ?FLOKK_DB:update(?BUCKET(Env), ID, Item, ?TWO_I).

delete(ID, Env) ->
  ?FLOKK_DB:delete(?BUCKET(Env), ID).

find(Index, Value, Env) ->
  ?FLOKK_DB:keys_by_index(?BUCKET(Env), Index, Value).

%% TODO
sale(_sID, _sEnv) ->
  {ok, []}.

% TODO
validate(_Item) ->
  true.
