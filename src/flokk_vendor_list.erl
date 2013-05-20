-module (flokk_vendor_list).

-export([init/2]).
-export([list/2]).
-export([body/3]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

list(Req, State) ->
  case flokk_vendor:list() of
    {ok, Vendors} ->
      {Vendors, Req, State};
    {error, _} ->
      {error, 500, Req}
  end.

body(Vendors, Req, State) ->
  Body = [
    {<<"vendors">>,
      [format_vendor(ID, Vendor, Req) || {ID, Vendor} <- Vendors]
    }
  ],

  %% Expose the create form
  Body1 = flokk_auth:build(<<"vendor.create">>, Req, Body, [
    {<<"create">>, [
      {<<"action">>, flokk_util:resolve(<<"vendors">>, Req)},
      {<<"method">>, <<"POST">>},
      {<<"input">>, [
        {<<"title">>, [
          {<<"type">>, <<"text">>}
        ]}
      ]}
    ]}
  ]),

  {Body1, Req, State}.

format_vendor(ID, Vendor, Req)->
  [
    {<<"href">>, flokk_util:resolve([<<"vendors">>, ID], Req)},
    {<<"title">>, proplists:get_value(<<"title">>, Vendor)}
  ].

ttl(Req, State)->
  {3600, Req, State}.
