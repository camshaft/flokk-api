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
-define(INDEX, <<"watches">>).

%% API.

item_watchers(Item, Env) ->
  ?FLOKK_DB:keys_by_index(?BUCKET(Env), ?INDEX, Item).

item_summary(Item, User, Env) ->
  case item_watchers(Item, Env) of
    {ok, List} ->
      {ok, {List, lists:member(User, List)}};
    Error ->
      Error
  end.

user_watches(User, Env) ->
  case ?FLOKK_DB:get_obj(?BUCKET(Env), User) of
    {ok, Obj} ->
      case ?FLOKK_DB:get_secondary_binary_index(?INDEX, Obj) of
        Watches when is_list(Watches) ->
          {ok, Watches};
        notfound ->
          {ok, []};
        Other ->
          Other
      end;
    {error, notfound} ->
      {ok, []};
    Error ->
      Error
  end.

user_summary(_User, _Env) ->
  {ok, 0}.

watch(Item, User, Env) ->
  case ?FLOKK_DB:get_obj(?BUCKET(Env), User) of
    {ok, Obj} ->
      update_item(Obj, Item, User, Env);
    {error, notfound} ->
      Obj = ?FLOKK_DB:new(?BUCKET(Env), User, Item),
      update_item(Obj, Item, User, Env);
    Error ->
      Error
  end.

update_item(Obj, Item, User, Env) ->
  Obj2 = ?FLOKK_DB:binary_index(?INDEX, [Item], Obj),
  case ?FLOKK_DB:put(Obj2) of
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
  case ?FLOKK_DB:get_obj(?BUCKET(Env), User) of
    {ok, Obj} ->
      Obj2 = ?FLOKK_DB:remove_binary_index(?INDEX, Obj),
      case ?FLOKK_DB:put(Obj2) of
        ok ->
          case item_summary(Item, User, Env) of
            %% We just added them to the list so they have to be here
            {ok, {List, true}} ->
              {ok, {lists:delete(User, List), false}};
            Other ->
              Other
          end;
        Error ->
          Error
      end;
    Error ->
      Error
  end.
