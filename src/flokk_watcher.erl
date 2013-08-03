-module(flokk_watcher).

%% API.
-export([item_watchers/2]).
-export([item_summary/2]).
-export([item_summary/3]).
-export([user_watches/2]).
-export([user_summary/2]).
-export([watch/3]).
-export([unwatch/3]).

-include("flokk.hrl").

%% Bucket name.
-define(BUCKET(Env), <<"fk_watcher_", Env/binary>>).

%% API.

item_watchers(Item, Env) ->
  case ?FLOKK_DB:get(?BUCKET(Env), Item) of
    {ok, Set} ->
      {ok, gb_sets:to_list(Set)};
    {error, notfound} ->
      {ok, []};
    Error ->
      Error
  end.

item_summary(Item, Env) ->
  item_summary(Item, undefined, Env).
item_summary(Item, User, Env) ->
  case ?FLOKK_DB:get(?BUCKET(Env), Item) of
    {ok, Set} when is_binary(User) ->
      {ok, gb_sets:size(Set), gb_sets:is_member(User, Set)};
    {ok, Set} ->
      {ok, gb_sets:size(Set)};
    {error, notfound} when is_binary(User) ->
      {ok, 0, false};
    {error, notfound} ->
      {ok, 0};
    Error ->
      Error
  end.

user_watches(_User, _Env) ->
  {ok, []}.

user_summary(_User, _Env) ->
  {ok, 0}.

watch(Item, User, Env) ->
  case ?FLOKK_DB:get(?BUCKET(Env), Item) of
    {ok, Set} ->
      NewSet = gb_sets:add_element(User, Set),
      return_list(?FLOKK_DB:update(?BUCKET(Env), Item, NewSet));
    {error, notfound} ->
      Set = gb_sets:from_list([User]),
      return_list(?FLOKK_DB:update(?BUCKET(Env), Item, Set));
    Error ->
      Error
  end.

unwatch(Item, User, Env) ->
  case ?FLOKK_DB:get(?BUCKET(Env), Item) of
    {ok, Set} ->
      NewSet = gb_sets:del_element(User, Set),
      return_list(?FLOKK_DB:update(?BUCKET(Env), Item, NewSet));
    Error ->
      Error
  end.

return_list({ok, Set}) ->
  {ok, gb_sets:to_list(Set)};
return_list(Error) ->
  Error.
