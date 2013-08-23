-module(flokk_vendor_list).

-export([init/2]).
-export([list/2]).
-export([body/3]).
-export([ttl/2]).

init(Req, _Opts) ->
  {ok, Req, []}.

list(Req, State) ->
  Response = flokk_vendor:list(cowboy_env:get(Req)),
  {Response, Req, State}.

body(Vendors, Req, State) ->
  Body = [
    {<<"vendors">>,
      [format_vendor(ID, Vendor, Req) || {ID, Vendor} <- Vendors]
    }
  ],

  %% Expose the create form
  Body1 = cowboy_resource_builder:authorize(<<"vendor.create">>, Req, Body, [
    {<<"create">>, [
      {<<"action">>, cowboy_base:resolve(<<"vendors">>, Req)},
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
  Title = fast_key:get(<<"title">>, Vendor),
  [
    {<<"href">>, cowboy_base:resolve([<<"vendors">>, ID], Req)},
    {<<"title">>, Title}
  ].

ttl(Req, State)->
  {3600, Req, State}.
