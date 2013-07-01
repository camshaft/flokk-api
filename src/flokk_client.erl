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

-record (state, {
  db,
  flokk_client_id = simple_env:get_binary("FLOKK_CLIENT_ID", <<"flokk-ui">>),
  flokk_client_secret = simple_env:get_binary("FLOKK_CLIENT_SECRET")
}).

%% Bucket name.
-define(BUCKET, <<"fk_client">>).

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
  {ok, #state{db=DB}}.

handle_call(stop, _, State) ->
  {stop, normal, stopped, State};
handle_call(list, _, State) ->
  % Response = DB:list(?BUCKET),
  Response = [<<"flokk-ui">>],
  {reply, {ok, Response}, State};
handle_call({read, ID}, _, State = #state{flokk_client_id = ID, flokk_client_secret = Secret}) ->
  % Response = DB:get(?BUCKET, ID),
  Response = [
    {<<"name">>, <<"The Flokk">>},
    {<<"description">>, <<"UI">>},
    {<<"secret">>, Secret},
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
  {reply, {ok, Response}, State};
handle_call({create, _Item}, _, State) ->
  % Response = DB:post(?BUCKET, Item),
  Response = <<"new-client-id">>,
  {reply, {ok, Response}, State};
handle_call({update, _ID, _Item}, _, State) ->
  % Response = DB:put(?BUCKET, ID, Item),
  Response = ok,
  {reply, Response, State};
handle_call({delete, _ID}, _, State) ->
  % Response = DB:delete(?BUCKET, ID),
  {reply, ok, State};
handle_call({find, Query}, _, State = #state{flokk_client_id = FlokkID}) ->
  % Response = DB:get(?BUCKET, ID),
  Results = case fast_key:get(<<"id">>, Query) of
    FlokkID = ID ->
      [ID];
    _ ->
      []
  end,
  {reply, {ok, Results}, State};
handle_call(ping, _, DB) ->
  {reply, DB:ping(), DB};
handle_call(_, _, State) ->
  {reply, ignore, State}.

handle_cast(_, State) ->
  {noreply, State}.

handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal.
