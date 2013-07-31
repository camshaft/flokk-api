-module (flokk_category_items).

-export([init/2]).
-export([call/2]).
-export([body/3]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

call(Req, State) ->
  {ID, Req} = cowboy_req:binding(id, Req),
  case flokk_item:find([{<<"category">>, ID}]) of
    {error, not_found} ->
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
        {<<"href">>, cowboy_base:resolve([<<"items">>,ID], Req)}
      ] || ID <- Items
    ]}
  ],

  {Body, Req, State}.

ttl(Req, State)->
  {3600, Req, State}.
