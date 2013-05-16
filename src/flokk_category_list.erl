-module (flokk_category_list).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, _) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  Categories = flokk_category:list(),
  Body = [
    {<<"categories">>,
      [format_category(ID, Category, Req) || {ID, Category} <- Categories]
    }
  ],

  %% TODO if scope `category.create` add the `create` action

  Req1 = cowboy_req:set_meta(body, Body, Req),
  Req2 = cowboy_req:set_meta(ttl, <<"3600">>, Req1),
  {ok, Req2, State}.

format_category(ID, Category, Req)->
  [
    {<<"title">>, proplists:get_value(<<"title">>, Category)},
    {<<"href">>, flokk_util:resolve(<<"categories">>, ID, Req)}
  ].

terminate(_Reason, _Req, _State) ->
  ok.
