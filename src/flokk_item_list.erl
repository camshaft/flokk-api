-module(flokk_item_list).

-export([init/2]).
-export([list/2]).
-export([body/3]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

list(Req, State) ->
  Response = flokk_item:list(cowboy_env:get(Req)),
  {Response, Req, State}.

body(Items, Req, State) ->
  P = presenterl:create(),

  P ! [
    {<<"items">>,
      [format_item(ID, Req) || ID <- Items]
    }
  ],

  presenterl:conditional([
    cowboy_resource_owner:is_authorized(<<"item.create">>, Req)
  ], fun () ->
    % TODO use batch
    {ok, Categories} = flokk_category:list(cowboy_env:get(Req)),
    {ok, Vendors} = flokk_vendor:list(cowboy_env:get(Req)),
    [
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
                {<<"prompt">>, fast_key:get(<<"title">>, Category)},
                {<<"value">>, ID}
              ] || {ID, Category} <- Categories
            ]}
          ]},
          {<<"publisher">>, [
            {<<"type">>, <<"select">>},
            {<<"required">>, true},
            {<<"options">>, [
              [
                {<<"prompt">>, fast_key:get(<<"name">>, Vendor)},
                {<<"value">>, ID}
              ] || {ID, Vendor} <- Vendors
            ]}
          ]},
          {<<"images">>, [
            {<<"type">>, <<"url">>},
            {<<"required">>, true},
            {<<"multiple">>, true}
          ]}
        ]}
      ]}
    ]
  end, P),

  Body = presenterl:encode(P),

  {Body, Req, State}.

format_item(ID, Req)->
  [
    {<<"href">>, cowboy_base:resolve([<<"items">>, ID], Req)}
  ].

ttl(Req, State)->
  {3600, Req, State}.
