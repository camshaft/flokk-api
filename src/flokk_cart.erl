-module(flokk_cart).

%% API.
-export([read/2]).
-export([set/4]).
-export([remove/3]).

-include("flokk.hrl").

%% Bucket name.
-define(BUCKET(Env), <<"fk_cart_", Env/binary>>).
-define(KEY(User, Item), <<User/binary, ":", Item/binary>>).
-define(INDEX_ITEM(Item), <<"user_", Item/binary>>).
-define(INDEX_USER(User), <<"item_", User/binary>>).

%% API.

read(User, Env) ->
  case ?FLOKK_DB:values_by_index(?BUCKET(Env), ?INDEX_USER(User), <<1>>) of
    {ok, Items} ->
      {ok, format(Items, User, [])};
    Error ->
      Error
  end.

set(User, Item, Quantity, Env) ->
  Obj = ?FLOKK_DB:new(?BUCKET(Env), ?KEY(User, Item), Quantity),
  Obj2 = ?FLOKK_DB:binary_index(?INDEX_ITEM(Item), [<<1>>], Obj),
  Obj3 = ?FLOKK_DB:binary_index(?INDEX_USER(User), [<<1>>], Obj2),
  case ?FLOKK_DB:put(Obj3) of
    ok ->
      read(User, Env);
    Error ->
      Error
  end.

remove(User, Item, Env) ->
  case ?FLOKK_DB:delete(?BUCKET(Env), ?KEY(User, Item)) of
    ok ->
      read(User, Env);
    Error ->
      Error
  end.

format([], _, Acc) ->
  Acc;
format([{Key, Quantity}|Items], User, Acc) ->
  Length = byte_size(User),
  <<User:Length/binary, ":", Item/binary>> = Key,
  format(Items, User, [{Item, Quantity}|Acc]).
