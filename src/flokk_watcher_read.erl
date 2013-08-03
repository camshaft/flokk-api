-module(flokk_watcher_read).

-export([init/2]).
-export([read/3]).
-export([body/4]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

read(ItemID, Req, State) ->
  OwnerID = cowboy_resource_owner:owner_id(Req),
  Response = flokk_watcher:item_summary(ItemID, OwnerID, cowboy_env:get(Req)),
  {Response, Req, State}.

body(ItemID, {Count, IsWatching}, Req, State) ->

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
  ], fun() ->
    {ok, Watchers} = flokk_watcher:item_watchers(ItemID, cowboy_env:get(Req)),
    [
      {<<"watchers">>,
        [
          [
            {<<"href">>, cowboy_base:resolve([<<"users">>, Watcher], Req)}
          ]
        || Watcher <- Watchers]
      }
    ] 
  end, P),

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
