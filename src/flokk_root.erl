-module (flokk_root).

-export([init/2]).
-export([body/2]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

body(Req, State) ->
  Body = [
    % {<<"sales">>, [
    %   {<<"href">>, cowboy_base:resolve(<<"sales">>, Req)},
    %   {<<"title">>, <<"Sales">>}
    % ]},
    {<<"categories">>, [
      {<<"href">>, cowboy_base:resolve(<<"categories">>, Req)},
      {<<"title">>, <<"Categories">>}
    ]},
    {<<"vendors">>, [
      {<<"href">>, cowboy_base:resolve(<<"vendors">>, Req)},
      {<<"title">>, <<"Vendors">>}
    ]}
  ],

  %% User specific links
  Body2 = case cowboy_resource_owner:owner_id(Req) of
    undefined -> Body;
    UserID ->
      [
        {<<"account">>, [
          {<<"href">>, cowboy_base:resolve([<<"users">>,UserID], Req)}
        ]}
      |Body]
  end,

  %% Auth links
  Body3 = cowboy_resource_builder:authorize(<<"user">>, Req, Body2, [
    {<<"users">>, [
      {<<"href">>, cowboy_base:resolve(<<"users">>, Req)}
    ]}
  ]),
  Body4 = cowboy_resource_builder:authorize(<<"client">>, Req, Body3, [
    {<<"clients">>, [
      {<<"href">>, cowboy_base:resolve(<<"clients">>, Req)}
    ]}
  ]),

  %% Item listing
  Body5 = cowboy_resource_builder:authorize(<<"item.list">>, Req, Body4, [
    {<<"items">>, [
      {<<"href">>, cowboy_base:resolve(<<"items">>, Req)}
    ]}
  ]),

  %% Item listing
  Body6 = cowboy_resource_builder:authorize(<<"cart.read">>, Req, Body5, fun (UserID) ->
      [
        {<<"cart">>, [
          {<<"href">>, cowboy_base:resolve([<<"carts">>, UserID], Req)}
        ]}
      ]
  end),

  {Body6, Req, State}.

ttl(Req, State)->
  {3600, Req, State}.
