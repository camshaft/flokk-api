-module(flokk_item_sale).

-export([init/2]).
-export([call/2]).
-export([body/3]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

call(Req, State) ->
  {ID, Req} = cowboy_req:binding(id, Req),
  case flokk_item:sale(ID, cowboy_env:get(Req)) of
    {ok, Sale} ->
      {{ok, {ID, Sale}}, Req, State};
    Error ->
      {Error, Req, State}
  end.

body({ID, Sale}, Req, State) ->
  ItemUrl = cowboy_base:resolve([<<"items">>, ID], Req),
  Ending = fast_key:get(<<"ending">>, Sale),
  Price = fast_key:get(<<"price">>, Sale, 0),

  P = presenterl:create(),

  P ! [
    {<<"profile">>, [
      {<<"href">>, <<"http://alps.io/schema.org/Offer.xml">>}
    ]},
    {<<"itemOffered">>, [
      {<<"href">>, ItemUrl}
    ]}
  ],

  % availability
  % availabilityEnds
  % availabilityStarts
  % category
  % deliveryLeadTime
  % eligibleDuration
  % eligibleQuantity
  % inventoryLevel
  % priceSpecification
  %   maxPrice
  %   minPrice
  %   price
  %   priceCurrency
  %   valueAddedTaxIncluded
  % validFrom
  % validThrough


  presenterl:conditional([
    Ending =/= undefined
  ], [
    {<<"ending">>, Ending},
    {<<"price">>, Price}
  ], P),

  %% TODO show the sale stats
  presenterl:conditional([
    cowboy_resource_owner:is_authorized(<<"sale.info">>, Req)
  ], [

  ], P),

  %% TODO show if we have stock
  presenterl:conditional([
    cowboy_resource_owner:is_authorized(<<"cart.update">>, Req)
  ], fun() ->
    OwnerID = cowboy_resource_owner:owner_id(Req),
    [
      {<<"purchase">>, [
        {<<"action">>, cowboy_base:resolve([<<"carts">>, OwnerID], Req)},
        {<<"method">>, <<"POST">>},
        {<<"input">>, [
          {<<"action">>, [
            {<<"type">>, <<"hidden">>},
            {<<"value">>, <<"add">>}
          ]},
          {<<"offer">>, [
            {<<"type">>, <<"hidden">>},
            {<<"value">>, ItemUrl}
          ]},
          {<<"quantity">>, [
            {<<"type">>, <<"select">>},
            {<<"prompt">>, <<"Quantity">>},
            {<<"value">>, 1},
            %% TODO limit to stock
            {<<"options">>, [
              [
                {<<"value">>, 1}
              ],
              [
                {<<"value">>, 2}
              ],
              [
                {<<"value">>, 3}
              ]
            ]}
          ]}
        ]}
      ]}
    ]
  end, P),

  Body = presenterl:encode(P),

  {Body , Req, State}.

%% We'll need to figure out a good ttl
ttl(Req, State)->
  {60, Req, State}.
