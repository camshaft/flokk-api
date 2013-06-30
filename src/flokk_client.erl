-module (flokk_client).

%% API.
-export([start_link/1]).
-export([stop/0]).
-export([available/0]).
-export([list/0]).
-export([read/1]).
-export([create/1]).
-export([update/2]).
-export([delete/1]).
-export([find/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Bucket name.
-define(BUCKET, <<"fk_client">>).

-define(SECRET, <<"08950e7d158605b4d9336721fc014269adddab1d95fcc28ce2be87122daeef55">>).

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

create(Item) ->
  gen_server:call(?MODULE, {create, Item}).

update(ID, Item) ->
  gen_server:call(?MODULE, {update, ID, Item}).

delete(ID) ->
  gen_server:call(?MODULE, {delete, ID}).

find(Query) ->
  gen_server:call(?MODULE, {find, Query}).


%% gen_server.

init(DB) ->
  {ok, DB}.

handle_call(stop, _, DB) ->
  {stop, normal, stopped, DB};
handle_call(list, _, DB) ->
  % Response = DB:list(?BUCKET),
  Response = [],
  {reply, {ok, Response}, DB};
handle_call({read, _ID}, _, DB) ->
  % Response = DB:get(?BUCKET, ID),
  Response = [
    {<<"name">>, <<"The Flokk">>},
    {<<"description">>, <<"UI">>},
    {<<"secret">>, ?SECRET},
    {<<"redirect_uri">>, [
      <<"http://localhost:5000">>,
      <<"https://test.theflokk.com">>
    ]},
    {<<"internal">>, true},
    {<<"scopes">>, [
      <<"user.email">>,
      <<"user.image">>,
      <<"user.name">>
    ]},
    {<<"optional_scopes">>, [
      <<"user.birthday">>
    ]}
  ],
  {reply, {ok, Response}, DB};
handle_call({create, _Item}, _, DB) ->
  % Response = DB:post(?BUCKET, Item),
  Response = <<"new-client-id">>,
  {reply, {ok, Response}, DB};
handle_call({update, _ID, _Item}, _, DB) ->
  % Response = DB:put(?BUCKET, ID, Item),
  Response = ok,
  {reply, Response, DB};
handle_call({delete, _ID}, _, DB) ->
  % Response = DB:delete(?BUCKET, ID),
  {reply, ok, DB};
handle_call({find, Query}, _, DB) ->
  % Response = DB:get(?BUCKET, ID),
  Results = [
    fast_key:get(<<"id">>, Query, <<"1">>)
  ],
  {reply, {ok, Results}, DB};
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
