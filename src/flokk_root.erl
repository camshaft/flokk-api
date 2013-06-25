-module (flokk_root).

-export([init/2]).
-export([body/2]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

body(Req, State) ->
  Body = [
    % {<<"sales">>, [
    %   {<<"href">>, cowboy_base:resolve(<<"sales">>, Req)},
    %   {<<"title">>, <<"Sales">>}
    % ]},
    {<<"categories">>, [
      {<<"href">>, cowboy_base:resolve(<<"categories">>, Req)},
      {<<"title">>, <<"Categories">>}
    ]},
    {<<"vendors">>, [
      {<<"href">>, cowboy_base:resolve(<<"vendors">>, Req)},
      {<<"title">>, <<"Vendors">>}
    ]}
  ],

  %% User specific links
  Body2 = case flokk_auth:user_id(Req) of
    undefined -> Body;
    UserID ->
      [
        {<<"account">>, [
          {<<"href">>, cowboy_base:resolve([<<"users">>,UserID], Req)}
        ]}
      |Body]
  end,

  {Body2, Req, State}.

ttl(Req, State)->
  {3600, Req, State}.
