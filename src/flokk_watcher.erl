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

item_summary(Item, User, Env) ->
  case ?FLOKK_DB:get(?BUCKET(Env), Item) of
    {ok, Set} ->
      return_list({ok, Set}, Set, User);
    {error, notfound} ->
      {ok, {0, false}};
    Error ->
      Error
  end.

user_watches(_User, _Env) ->
  {ok, []}.

user_summary(_User, _Env) ->
  {ok, 0}.

%% TODO use crdts
watch(Item, User, Env) ->
  case ?FLOKK_DB:get(?BUCKET(Env), Item) of
    {ok, Set} ->
      NewSet = gb_sets:add_element(User, Set),
      return_list(?FLOKK_DB:update(?BUCKET(Env), Item, NewSet), NewSet, User);
    {error, notfound} ->
      Set = gb_sets:from_list([User]),
      return_list(?FLOKK_DB:update(?BUCKET(Env), Item, Set), Set, User);
    Error ->
      Error
  end.

unwatch(Item, User, Env) ->
  case ?FLOKK_DB:get(?BUCKET(Env), Item) of
    {ok, Set} ->
      NewSet = gb_sets:del_element(User, Set),
      return_list(?FLOKK_DB:update(?BUCKET(Env), Item, NewSet), NewSet, User);
    Error ->
      Error
  end.

return_list({ok, _}, Set, User) ->
  {ok, {gb_sets:size(Set), gb_sets:is_member(User, Set)}};
return_list(Error, _, _User) ->
  Error.
