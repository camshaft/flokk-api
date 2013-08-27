-module(flokk_user_watches_read).

-export([init/2]).
-export([read/3]).
-export([body/4]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

read(ID, Req, State) ->
  Response = flokk_watcher:user_watches(ID, cowboy_env:get(Req)),
  {Response, Req, State}.

body(ID, Watches, Req, State) ->
  IsOwner = case cowboy_resource_owner:owner_id(Req) of
    ID -> true;
    undefined -> true;
    _ -> false
  end,

  P = presenterl:create(),

  presenterl:conditional([
    IsOwner,
    cowboy_resource_owner:is_authorized([<<"user.watches">>], Req)
  ], [
    {<<"watches">>, [
      [
        {<<"href">>, cowboy_base:resolve([<<"items">>, Item], Req)}
      ]
    || Item <- Watches]}
  ], P),

  Body = presenterl:encode(P),

  {Body, Req, State}.

ttl(Req, State) ->
  {3600, Req, State}.
