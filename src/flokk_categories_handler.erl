-module (flokk_categories_handler).

-export([init/3]).
-export([resource_exists/2]).
-export([content_types_provided/2]).
-export([to_json/2]).

init(_Transport, _Req, _State) ->
  {upgrade, protocol, cowboy_rest}.

resource_exists(Req, State) ->
  %% TODO look up in database
  Categories = [
    {<<"href">>, flokk_util:resolve(<<"/categories">>, Req)},
    {<<"root">>, [{<<"href">>, flokk_util:resolve(<<"/">>, Req)}]},
    {<<"home">>, [
      {<<"href">>, flokk_util:resolve(<<"/categories/home">>, Req)},
      {<<"title">>, <<"Home">>}
    ]},
    {<<"bath">>, [
      {<<"href">>, flokk_util:resolve(<<"/categories/bath">>, Req)},
      {<<"title">>, <<"Bath">>}
    ]},
    {<<"bed">>, [
      {<<"href">>, flokk_util:resolve(<<"/categories/bed">>, Req)},
      {<<"title">>, <<"Bed">>}
    ]}
  ],
  {true, Req, [{categories, Categories}|State]}.

%% TODO add expires
%% TODO generate etag

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, to_json}
  ], Req, State}.

to_json(Req, State) ->
  Categories = proplists:get_value(categories, State),
  {jsx:encode(Categories), Req, State}.
