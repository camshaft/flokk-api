-module(flokk_vendor).

%% API.
-export([list/1]).
-export([read/2]).
-export([create/2]).
-export([update/3]).
-export([delete/2]).
-export([find/3]).
-export([validate/1]).

-include("flokk.hrl").

%% Bucket name.
-define(BUCKET(Env), <<"fk_vendor_", Env/binary>>).

-define(TWO_I, []).

%% API.

list(Env) ->
  % TODO page this
  ?FLOKK_DB:values(?BUCKET(Env)).

read(ID, Env) ->
  ?FLOKK_DB:get(?BUCKET(Env), ID).

create(Vendor, Env) ->
  ?FLOKK_DB:create(?BUCKET(Env), Vendor, ?TWO_I).

update(ID, Vendor, Env) ->
  ?FLOKK_DB:update(?BUCKET(Env), ID, Vendor, ?TWO_I).

delete(ID, Env) ->
  ?FLOKK_DB:delete(?BUCKET(Env), ID).

find(Index, Value, Env) ->
  ?FLOKK_DB:keys_by_index(?BUCKET(Env), Index, Value).

% TODO
validate(_Item) ->
  true.
