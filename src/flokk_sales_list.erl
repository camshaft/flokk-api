-module(flokk_sales_list).

-export([init/2]).
-export([list/2]).
-export([body/3]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

list(Req, State) ->
  case flokk_category:list(cowboy_env:get(Req)) of
    {ok, Categories} ->
      Response = [begin
        %% TODO use batch
        CategoryURL = cowboy_base:resolve([<<"categories">>, ID], Req),
        {ok, Items} = flokk_item_scoreboard:top(ID),
        {CategoryURL, Items}
      end || {ID, _} <- Categories],
      {{ok, Response}, Req, State};
    _ ->
      {{ok, []}, Req, State}
  end.

body(Categories, Req, State) ->

  Body = [
    {<<"sections">>, [
      [
        {<<"image">>, <<"https://d30wvy161n1c3v.cloudfront.net/livingroom.svg">>},
        {<<"main">>, <<"https://d30wvy161n1c3v.cloudfront.net/1c5a09ae839d0e0086b134d0ac3c6416-clock.jpg">>},
        {<<"category">>, Category},
        {<<"items">>, [
          [
            {<<"href">>, cowboy_base:resolve([<<"items">>, fast_key:get(<<"item">>, Item)], Req)}
          ] || Item <- Items
        ]}
      ]
    || {Category, Items} <- Categories]}
  ],

  {Body, Req, State}.

ttl(Req, State)->
  {3600, Req, State}.
