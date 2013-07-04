-module (flokk_item_list).

-export([init/2]).
-export([list/2]).
-export([body/3]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

list(Req, State) ->
  case flokk_item:list() of
    {ok, Items} ->
      {Items, Req, State};
    {error, _} ->
      {error, 500, Req}
  end.

body(Items, Req, State) ->
  Body = [
    {<<"items">>,
      [format_item(ID, Item, Req) || {ID, Item} <- Items]
    }
  ],

  {ok, Categories} = flokk_category:list(),
  {ok, Vendors} = flokk_vendor:list(),

  io:format("~p~n", [cowboy_resource_owner:scopes(Req)]),

  %% Expose the create form
  Body1 = cowboy_resource_builder:authorize(<<"item.create">>, Req, Body, [
    {<<"create">>, [
      {<<"action">>, cowboy_base:resolve(<<"items">>, Req)},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        {<<"title">>, [
          {<<"type">>, <<"text">>}
        ]},
        {<<"description">>, [
          {<<"type">>, <<"text">>}
        ]},
        {<<"retail">>, [
          {<<"type">>, <<"currency">>}
        ]},
        {<<"max-discount">>, [
          {<<"type">>, <<"percent">>}
        ]},
        {<<"shipping">>, [
          {<<"type">>, <<"currency">>}
        ]},
        {<<"currency">>, [
          {<<"type">>, <<"select">>},
          {<<"options">>, [
            %% TODO pull this from somewhere
            [
              {<<"value">>, <<"USD">>}
            ]
          ]}
        ]},
        {<<"category">>, [
          {<<"type">>, <<"select">>},
          {<<"options">>, [
            [
              {<<"prompt">>, Title},
              {<<"value">>, ID}
            ] || {ID, Title} <- Categories
          ]}
        ]},
        {<<"publisher">>, [
          {<<"type">>, <<"select">>},
          {<<"options">>, [
            [
              {<<"prompt">>, Title},
              {<<"value">>, ID}
            ] || {ID, Title} <- Vendors
          ]}
        ]},
        {<<"image">>, [
          {<<"type">>, <<"url">>}
        ]}
      ]}
    ]}
  ]),

  {Body1, Req, State}.

format_item(ID, Item, Req)->
  [
    {<<"href">>, cowboy_base:resolve([<<"items">>, ID], Req)},
    {<<"title">>, fast_key:get(<<"title">>, Item)}
  ].

ttl(Req, State)->
  {3600, Req, State}.
