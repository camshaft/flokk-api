-module (flokk_root).

-export([init/2]).
-export([body/2]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

body(Req, State) ->
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
  Body2 = case flokk_auth:user_id(Req) of
    undefined -> Body;
    UserID ->
      [
        {<<"profile">>, [
          {<<"href">>, flokk_util:resolve([<<"users">>,UserID], Req)}
        ]}
      |Body]
  end,

  {Body2, Req, State}.

ttl(Req, State)->
  {3600, Req, State}.
