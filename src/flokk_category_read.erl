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
  URL = cowboy_base:resolve([<<"category">>,ID], Req),
  Title = fast_key:get(<<"title">>, Category, <<>>),

  P = presenterl:create(),

  P ! [
    {<<"title">>, Title},
    {<<"items">>, [
      {<<"href">>, cowboy_base:resolve([<<"categories">>,ID,<<"items">>], Req)}
    ]}
  ],

  presenterl:conditional([
    cowboy_resource_owner:is_authorized([<<"category.update">>], Req)
  ], [
    {<<"action">>, URL},
    {<<"method">>, <<"POST">>},
    {<<"input">>, [
      {<<"title">>, [
        {<<"type">>, <<"text">>},
        {<<"value">>, Title}
      ]}
    ]}
  ], P),

  presenterl:conditional([
    cowboy_resource_owner:is_authorized([<<"category.delete">>], Req)
  ], [
    {<<"action">>, URL},
    {<<"method">>, <<"DELETE">>}
  ], P),

  Body = presenterl:encode(P),

  {Body, Req, State}.

ttl(Req, State) ->
  {3600, Req, State}.
