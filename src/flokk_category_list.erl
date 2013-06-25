-module (flokk_category_list).

-export([init/2]).
-export([list/2]).
-export([body/3]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

list(Req, State) ->
  case flokk_category:list() of
    {ok, Categories} ->
      {Categories, Req, State};
    {error, _} ->
      {error, 500, Req}
  end.

body(Categories, Req, State) ->
  Body = [
    {<<"categories">>,
      [format_category(ID, Category, Req) || {ID, Category} <- Categories]
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
        ]}
      ]}
    ]}
  ]),

  {Body1, Req, State}.

format_category(ID, Category, Req)->
  [
    {<<"href">>, cowboy_base:resolve([<<"categories">>, ID], Req)},
    {<<"title">>, fast_key:get(<<"title">>, Category)},
    {<<"items">>, cowboy_base:resolve([<<"categories">>,ID,<<"items">>], Req)}
  ].

ttl(Req, State)->
  {3600, Req, State}.
