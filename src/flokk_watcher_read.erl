-module (flokk_watcher_read).

-export([init/2]).
-export([read/3]).
-export([body/4]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

read(ItemID, Req, State) ->
  case flokk_watcher:list(ItemID) of
    {error, not_found} ->
      {error, 404, Req};
    {error, _} ->
      {error, 500, Req};
    {ok, Watchers} ->
      {Watchers, Req, State}
  end.

body(ItemID, {Count, Watchers}, Req, State) ->

  Url = cowboy_base:resolve([<<"items">>, ItemID, <<"watchers">>], Req),

  P = presenterl:create(),

  P ! [
    {<<"item">>, [
      {<<"href">>, cowboy_base:resolve([<<"items">>, ItemID], Req)}
    ]},
    {<<"count">>, Count}
  ],

  presenterl:conditional([
    cowboy_resource_owner:is_authorized(<<"item.watchers">>, Req)
  ], fun() -> [
    {<<"watchers">>, 
      [
        [
          {<<"href">>, cowboy_base:resolve([<<"users">>, Watcher], Req)}
        ]
      || Watcher <- Watchers]
    }
  ] end, P),

  UserID = cowboy_resource_owner:owner_id(Req),
  IsWatching = lists:member(UserID, Watchers),
  IsAllowed = cowboy_resource_owner:is_authorized(<<"user.watches">>, Req),

  presenterl:conditional([
    IsAllowed,
    not IsWatching
  ], [
    {<<"watch">>, [
      {<<"action">>, Url},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        {<<"action">>, [
          {<<"type">>, <<"hidden">>},
          {<<"value">>, <<"watch">>}
        ]}
      ]}
    ]}
  ], P),

  presenterl:conditional([
    IsAllowed,
    IsWatching
  ], [
    {<<"unwatch">>, [
      {<<"action">>, Url},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        {<<"action">>, [
          {<<"type">>, <<"hidden">>},
          {<<"value">>, <<"unwatch">>}
        ]}
      ]}
    ]}
  ], P),

  Body = presenterl:encode(P),

  {Body, Req, State}.

ttl(Req, State) ->
  {3600, Req, State}.
