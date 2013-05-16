-module (flokk_root).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  %% TODO come up with a better way to build this data
  Body = [
    {<<"sales">>, [
      {<<"href">>, flokk_util:resolve(<<"categories/sales">>, Req)},
      {<<"title">>, <<"Sales">>}
    ]},
    {<<"categories">>, [
      {<<"href">>, flokk_util:resolve(<<"categories">>, Req)},
      {<<"title">>, <<"Categories">>}
    ]}
  ],

  %% User specific links
  Body2 = case cowboy_req:meta(user_id, Req, undefined) of
    {undefined, Req} -> Body;
    {UserID, Req} -> [{<<"profile">>, [{<<"href">>, flokk_util:resolve(<<"users/">>,UserID, Req)}]}|Body]
  end,

  %% Pass the body
  Req1 = cowboy_req:set_meta(body, Body2, Req),
  Req2 = cowboy_req:set_meta(ttl, <<"3600">>, Req1),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
