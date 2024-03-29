-module(flokk_vendor_items).

-export([init/2]).
-export([call/2]).
-export([body/3]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

call(Req, State) ->
  {ID, Req} = cowboy_req:binding(id, Req),
  Response = flokk_item:find(<<"vendor">>, ID, cowboy_env:get(Req)),
  {Response, Req, State}.

body(Items, Req, State) ->
  Body = [
    {<<"items">>, [
      [
        {<<"href">>, cowboy_base:resolve([<<"items">>,ID], Req)},
        {<<"title">>, fast_key:get(<<"title">>, Item)}
      ] || {ID, Item} <- Items
    ]}
  ],

  {Body, Req, State}.

ttl(Req, State)->
  {3600, Req, State}.
