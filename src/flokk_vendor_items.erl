-module (flokk_vendor_items).

-export([init/2]).
-export([call/2]).
-export([body/3]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

call(Req, State) ->
  {ID, Req} = cowboy_req:binding(id, Req),
  case flokk_vendor:items(ID) of
    {error, notfound} ->
      {false, Req, State};
    {error, _} ->
      {error, 500, Req};
    {ok, Items} ->
      {Items, Req, State}
  end.

body(Items, Req, State) ->
  Body = [
    {<<"items">>, [
      [
        {<<"href">>, flokk_util:resolve([<<"items">>,ID], Req)},
        {<<"title">>, proplists:get_value(<<"title">>, Item)}
      ] || {ID, Item} <- Items
    ]}
  ],

  {Body, Req, State}.

ttl(Req, State)->
  {3600, Req, State}.
