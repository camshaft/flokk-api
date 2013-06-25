-module (flokk_item_sale).

-export([init/2]).
-export([call/2]).
-export([body/3]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

call(Req, State) ->
  {ID, Req} = cowboy_req:binding(id, Req),
  case flokk_item:sale(ID) of
    {error, notfound} ->
      {false, Req, State};
    {error, _} ->
      {error, 500, Req};
    {ok, Sale} ->
      {{ID, Sale}, Req, State}
  end.

body({ID, Sale}, Req, State) ->
  URL = cowboy_base:resolve([<<"items">>,ID,<<"sale">>], Req),
  Price = fast_key:get(<<"price">>, Sale, 3999),

  Body = [
    {<<"item">>, [
      {<<"href">>, cowboy_base:resolve([<<"items">>,ID], Req)}
    ]},
    {<<"purchase">>, [
      {<<"action">>, URL},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        %% TODO
      ]}
    ]}
  ],

  Body1 = case fast_key:get(<<"ending">>, Sale) of
    undefined -> Body;
    Ending -> lists:concat([Body, [{<<"ending">>, Ending}, {<<"price">>, Price}]])
  end,

  %% TODO show the sale stats
  Body2 = cowboy_resource_builder:authorize(<<"sale.info">>, Req, Body1, [

  ]),

  {Body2, Req, State}.

%% We'll need to figure out a good ttl
ttl(Req, State)->
  {60, Req, State}.
