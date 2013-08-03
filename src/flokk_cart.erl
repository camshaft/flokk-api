-module(flokk_cart).

%% API.
-export([read/2]).
-export([initialize/2]).
-export([add/4]).
-export([remove/3]).

-include("flokk.hrl").

%% Bucket name.
-define(BUCKET(Env), <<"fk_cart_", Env/binary>>).

%% API.

read(ID, Env) ->
  ?FLOKK_DB:get(?BUCKET(Env), ID).

initialize(ID, Env) ->
  ?FLOKK_DB:update(?BUCKET(Env), ID, []).

add(ID, Item, Quantity, Env) ->
  case ?FLOKK_DB:get(?BUCKET(Env), ID) of
    {ok, Cart} ->
      NewAmount = Quantity + fast_key:get(Item, Cart, 0),
      UpdatedCart = fast_key:set(Item, NewAmount, Cart),

      ?FLOKK_DB:update(?BUCKET(Env), ID, UpdatedCart);
    Error ->
      Error
  end.

remove(ID, Item, Env) ->
  case ?FLOKK_DB:get(?BUCKET(Env), ID) of
    {ok, Cart} ->
      UpdatedCart = lists:keydelete(Item, 1, Cart),
      ?FLOKK_DB:update(?BUCKET(Env), ID, UpdatedCart);
    Error ->
      Error
  end.
