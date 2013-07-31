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
      [format_item(ID, Req) || ID <- Items]
    }
  ],

  {ok, Categories} = flokk_category:list(),
  {ok, Vendors} = flokk_vendor:list(),

  %% Expose the create form
  Body1 = cowboy_resource_builder:authorize(<<"item.create">>, Req, Body, [
    {<<"create">>, [
      {<<"action">>, cowboy_base:resolve(<<"items">>, Req)},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        {<<"name">>, [
          {<<"type">>, <<"text">>},
          {<<"required">>, true}
        ]},
        {<<"description">>, [
          {<<"type">>, <<"textarea">>},
          {<<"required">>, true}
        ]},
        {<<"retail">>, [
          {<<"type">>, <<"currency">>},
          {<<"required">>, true}
        ]},
        {<<"max-discount">>, [
          {<<"type">>, <<"percentage">>},
          {<<"required">>, true}
        ]},
        {<<"shipping">>, [
          {<<"type">>, <<"currency">>},
          {<<"required">>, true}
        ]},
        {<<"currency">>, [
          {<<"type">>, <<"select">>},
          {<<"required">>, true},
          {<<"options">>, [
            %% TODO pull this from somewhere
            [
              {<<"value">>, <<"USD">>}
            ]
          ]}
        ]},
        {<<"category">>, [
          {<<"type">>, <<"select">>},
          {<<"required">>, true},
          {<<"options">>, [
            [
              {<<"prompt">>, Title},
              {<<"value">>, ID}
            ] || {ID, Title} <- Categories
          ]}
        ]},
        {<<"publisher">>, [
          {<<"type">>, <<"select">>},
          {<<"required">>, true},
          {<<"options">>, [
            [
              {<<"prompt">>, Title},
              {<<"value">>, ID}
            ] || {ID, Title} <- Vendors
          ]}
        ]},
        {<<"images">>, [
          {<<"type">>, <<"url">>},
          {<<"required">>, true},
          {<<"multiple">>, true}
        ]}
      ]}
    ]}
  ]),

  {Body1, Req, State}.

format_item(ID, Req)->
  [
    {<<"href">>, cowboy_base:resolve([<<"items">>, ID], Req)}
  ].

ttl(Req, State)->
  {3600, Req, State}.
