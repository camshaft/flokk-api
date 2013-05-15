-module (flokk_category_handler).

-export([init/3]).
-export([resource_exists/2]).
-export([content_types_provided/2]).
-export([to_json/2]).

init(_Transport, _Req, _State) ->
  {upgrade, protocol, cowboy_rest}.

resource_exists(Req, State) ->
  %% TODO look up in database
  Category = [
    {<<"href">>, flokk_util:resolve(<<"/categories/__CATEGORY_ID__">>, Req)},
    % {<<"subscribe">>, flokk_util:resolve(<<"/categories/__CATEGORY_ID__">>, Req)}, %% TODO replace proto with ws
    {<<"root">>, [{<<"href">>,flokk_util:resolve(<<"/">>, Req)}]},
    {<<"categories">>, [{<<"href">>,flokk_util:resolve(<<"/categories">>, Req)}]},
    {<<"title">>, <<"__CATEGORY_TITLE__">>}
  ],
  {true, Req, [{category, Category}|State]}.

%% TODO add expires
%% TODO generate etag

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, to_json}
  ], Req, State}.

to_json(Req, State) ->
  Category = proplists:get_value(category, State),
  {jsx:encode(Category), Req, State}.
