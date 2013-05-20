-module (flokk_item_list).

-export([init/2]).
-export([list/2]).
-export([body/3]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

list(Req, State) ->
  Items = flokk_item:list(),
  {Items, Req, State}.

body(Items, Req, State) ->
  Body = [
    {<<"items">>,
      [format_item(ID, Item, Req) || {ID, Item} <- Items]
    }
  ],

  %% Expose the create form
  Body1 = flokk_auth:build(<<"item.create">>, Req, Body, [
    {<<"create">>, [
      {<<"action">>, flokk_util:resolve(<<"items">>, Req)},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        {<<"title">>, [
          {<<"type">>, <<"text">>}
        ]}
      ]}
    ]}
  ]),

  {Body1, Req, State}.

format_item(ID, Item, Req)->
  [
    {<<"href">>, flokk_util:resolve([<<"items">>, ID], Req)},
    {<<"title">>, proplists:get_value(<<"title">>, Item)}
  ].

ttl(Req, State)->
  {3600, Req, State}.
