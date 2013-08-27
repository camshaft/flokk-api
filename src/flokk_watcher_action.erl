-module(flokk_watcher_action).

-export([init/2]).
-export([scope/2]).
-export([validate/3]).
-export([watch/4]).
-export([unwatch/4]).

-define(SCOPE, <<"user.watches">>).

init(Req, _Opts) ->
  {ok, Req, []}.

scope(Req, State) ->
  {?SCOPE, Req, State}.

validate(_Body, Req, State) ->
  {true, Req, State}.

watch(ItemID, _Body, Req, State) ->
  response(watch, ItemID, Req, State).

unwatch(ItemID, _Body, Req, State) ->
  response(unwatch, ItemID, Req, State).

response(Action, ItemID, Req, State) ->
  %% Get the item to make sure it exists
  {ok, Item} = flokk_item:read(ItemID, cowboy_env:get(Req)),

  OwnerID = cowboy_resource_owner:owner_id(Req),
  {ok, Report = {Watchers, _}} = flokk_watcher:Action(ItemID, OwnerID, cowboy_env:get(Req)),
  URL = cowboy_base:resolve([<<"items">>, ItemID, <<"watchers">>], Req),
  UserWatches = cowboy_base:resolve([<<"users">>, OwnerID, <<"watches">>], Req),

  %% Set the new score based on the watcher count
  flokk_item_scoreboard:set(ItemID, length(Watchers), fast_key:get(<<"category">>, Item)),

  Req2 = cowboy_req:set_resp_header(<<"content-location">>, URL, Req),
  Req3 = cowboy_req:set_resp_header(<<"link">>, <<"<", UserWatches/binary, ">; rel=invalidates">>, Req2),
  flokk_watcher_read:body(ItemID, Report, Req3, State).
