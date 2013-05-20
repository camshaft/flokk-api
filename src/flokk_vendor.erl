-module (flokk_vendor).

%% API.
-export([start_link/1]).
-export([stop/0]).
-export([available/0]).
-export([list/0]).
-export([read/1]).
-export([create/1]).
-export([update/2]).
-export([delete/1]).
-export([items/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Bucket name.
-define(BUCKET, <<"fk_vendor">>).

%% API.

start_link(DB) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, DB, []).

stop() ->
  gen_server:call(?MODULE, stop).

available() ->
  gen_server:call(?MODULE, ping) =:= pong.

list() ->
  gen_server:call(?MODULE, list).

read(ID) ->
  gen_server:call(?MODULE, {read, ID}).

create(Vendor) ->
  gen_server:call(?MODULE, {create, Vendor}).

update(ID, Vendor) ->
  gen_server:call(?MODULE, {update, ID, Vendor}).

delete(ID) ->
  gen_server:call(?MODULE, {delete, ID}).

items(ID) ->
  gen_server:call(?MODULE, {items, ID}).


%% gen_server.

init(DB) ->
  {ok, DB}.

handle_call(stop, _, DB) ->
  {stop, normal, stopped, DB};
handle_call(list, _, DB) ->
  % Response = DB:list(?BUCKET),
  Response = [
    {<<"scott-n-dave">>, [
      {<<"title">>, <<"Scott 'n Dave">>}
    ]},
    {<<"wicked-wicker">>, [
      {<<"title">>, <<"Wicked Wicker">>}
    ]}
  ],
  {reply, {ok, Response}, DB};
handle_call({read, _ID}, _, DB) ->
  % Response = DB:get(?BUCKET, ID),
  Response = [
    {<<"title">>, <<"Scott 'n Dave">>},
    {<<"description">>, <<"We specialize in lame prints">>}
  ],
  {reply, {ok, Response}, DB};
handle_call({create, _Category}, _, DB) ->
  % Response = DB:post(?BUCKET, Category),
  Response = <<"new-vendor-id">>,
  {reply, {ok, Response}, DB};
handle_call({update, _ID, _Category}, _, DB) ->
  % Response = DB:put(?BUCKET, ID, Category),
  Response = ok,
  {reply, Response, DB};
handle_call({delete, _ID}, _, DB) ->
  % Response = DB:delete(?BUCKET, ID),
  {reply, ok, DB};
handle_call({items, _ID}, _, DB) ->
  % Response = DB:delete(?BUCKET, ID),
  Response = [
    {<<"1">>, [{<<"title">>, <<"Lame Print 1">>}]},
    {<<"2">>, [{<<"title">>, <<"Lame Print 2">>}]},
    {<<"3">>, [{<<"title">>, <<"Lame Print 3">>}]}
  ],
  {reply, {ok, Response}, DB};
handle_call(ping, _, DB) ->
  {reply, DB:ping(), DB};
handle_call(_, _, DB) ->
  {reply, ignore, DB}.

handle_cast(_, DB) ->
  {noreply, DB}.

handle_info(_, DB) ->
  {noreply, DB}.

terminate(_Reason, _DB) ->
  ok.

code_change(_OldVsn, DB, _Extra) ->
  {ok, DB}.

%% Internal.
