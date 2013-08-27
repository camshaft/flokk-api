-module(flokk_category_list).

-export([init/2]).
-export([list/2]).
-export([body/3]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

list(Req, State) ->
  Response = flokk_category:list(cowboy_env:get(Req)),
  {Response, Req, State}.

body(Categories, Req, State) ->
  %% Here we can lookup the popularity of each category and sort by that
  SortedCategories = lists:sort(fun({_, C1}, {_, C2}) ->
    fast_key:get(<<"title">>, C1) =< fast_key:get(<<"title">>, C2)
  end, Categories),

  Body = [
    {<<"categories">>,
      [format_category(ID, Category, Req) || {ID, Category} <- SortedCategories]
    }
  ],

  %% Expose the create form
  Body1 = cowboy_resource_builder:authorize(<<"category.create">>, Req, Body, [
    {<<"create">>, [
      {<<"action">>, cowboy_base:resolve(<<"categories">>, Req)},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        {<<"title">>, [
          {<<"type">>, <<"text">>}
        ]},
        {<<"label">>, [
          {<<"type">>, <<"url">>}
        ]},
        {<<"promo">>, [
          {<<"type">>, <<"url">>}
        ]}
      ]}
    ]}
  ]),

  {Body1, Req, State}.

format_category(ID, Category, Req)->
  Title = fast_key:get(<<"title">>, Category),
  [
    {<<"href">>, cowboy_base:resolve([<<"categories">>, ID], Req)},
    {<<"title">>, Title},
    {<<"items">>, cowboy_base:resolve([<<"categories">>, ID, <<"items">>], Req)}
  ].

ttl(Req, State)->
  {3600, Req, State}.
