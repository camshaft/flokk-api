-module (flokk_category_read).

-export([init/2]).
-export([read/3]).
-export([body/4]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

read(ID, Req, State) ->
  case flokk_category:read(ID) of
    {error, notfound} -> {error, 404, Req};
    {error, _} -> {error, 500, Req};
    {ok, Category} -> {Category, Req, State}
  end.

body(ID, Category, Req, State) ->
  URL = flokk_util:resolve([<<"category">>,ID], Req),
  Title = proplists:get_value(<<"title">>, Category, <<>>),

  Body = [
    {<<"title">>, Title},
    {<<"items">>, [
      {<<"href">>, flokk_util:resolve([<<"category">>,ID,<<"items">>], Req)}
    ]}
  ],

  Body1 = flokk_auth:build(<<"category.update">>, Req, Body, [
    {<<"action">>, URL},
    {<<"method">>, <<"POST">>},
    {<<"input">>, [
      {<<"title">>, [
        {<<"type">>, <<"text">>},
        {<<"value">>, Title}
      ]}
    ]}
  ]),

  Body2 = flokk_auth:build(<<"category.delete">>, Req, Body1, [
    {<<"action">>, URL},
    {<<"method">>, <<"DELETE">>}
  ]),

  {Body2, Req, State}.

ttl(Req, State) ->
  {3600, Req, State}.
