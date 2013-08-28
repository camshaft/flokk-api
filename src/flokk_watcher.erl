-module(flokk_watcher).

%% API.
-export([item_watchers/2]).
-export([item_summary/3]).
-export([user_watches/2]).
-export([user_summary/2]).
-export([watch/3]).
-export([unwatch/3]).

-include("flokk.hrl").

%% Bucket name.
-define(BUCKET(Env), <<"fk_watcher_", Env/binary>>).
-define(KEY(Item, User), <<Item/binary, ":", User/binary>>).
-define(INDEX_ITEM(Item), <<"user_", Item/binary>>).
-define(INDEX_USER(User), <<"item_", User/binary>>).

%% API.

item_watchers(Item, Env) ->
  case ?FLOKK_DB:keys_by_index(?BUCKET(Env), ?INDEX_ITEM(Item), <<1>>) of
    {ok, Users} ->
      {ok, format(Users, [], user)};
    Error ->
      Error
  end.

item_summary(Item, User, Env) ->
  case item_watchers(Item, Env) of
    {ok, List} ->
      {ok, {List, lists:member(User, List)}};
    Error ->
      Error
  end.

user_watches(User, Env) ->
  case ?FLOKK_DB:keys_by_index(?BUCKET(Env), ?INDEX_USER(User), <<1>>) of
    {ok, Items} ->
      {ok, format(Items, [], item)};
    Error ->
      Error
  end.

user_summary(_User, _Env) ->
  {ok, 0}.

watch(Item, User, Env) ->
  Obj = ?FLOKK_DB:new(?BUCKET(Env), ?KEY(Item, User), <<1>>),
  Obj2 = ?FLOKK_DB:binary_index(?INDEX_ITEM(Item), [<<1>>], Obj),
  Obj3 = ?FLOKK_DB:binary_index(?INDEX_USER(User), [<<1>>], Obj2),
  case ?FLOKK_DB:put(Obj3) of
    ok ->
      case item_summary(Item, User, Env) of
        %% We just added them to the list so they have to be here
        {ok, {List, false}} ->
          {ok, {[User|List], true}};
        Other ->
          Other
      end;
    Error ->
      Error
  end.

unwatch(Item, User, Env) ->
  case ?FLOKK_DB:delete(?BUCKET(Env), ?KEY(Item, User)) of
    ok ->
      case item_summary(Item, User, Env) of
        %% We just removed them from the list so they can't to be here
        {ok, {List, true}} ->
          {ok, {lists:delete(User, List), false}};
        Other ->
          Other
      end;
    Error ->
      Error
  end.

format([], Acc, _Prop) ->
  Acc;
format([Key|Keys], Acc, item) ->
  [Item, _] = binary:split(Key, <<":">>),
  format(Keys, [Item|Acc], item);
format([Key|Keys], Acc, user) ->
  [_, User] = binary:split(Key, <<":">>),
  format(Keys, [User|Acc], user).
