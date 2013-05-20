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
  Body1 = flokk_auth:build(<<"category.create">>, Req, Body, [
    {<<"create">>, [
      {<<"action">>, flokk_util:resolve(<<"categories">>, Req)},
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
    {<<"href">>, flokk_util:resolve([<<"categories">>, ID], Req)},
    {<<"title">>, proplists:get_value(<<"title">>, Category)}
  ].

ttl(Req, State)->
  {3600, Req, State}.
