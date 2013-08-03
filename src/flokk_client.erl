-module(flokk_client).

%% API.
-export([start_link/0]).
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

-record(state, {
  db,
  flokk_ui_client_id = simple_env:get_binary("FLOKK_UI_CLIENT_ID", <<"flokk-ui">>),
  flokk_ui_client_secret = simple_env:get_binary("FLOKK_UI_CLIENT_SECRET"),
  flokk_ui_client_scopes = binary:split(simple_env:get_binary("FLOKK_UI_CLIENT_SCOPES", <<>>), <<",">>, [global]),
  flokk_admin_client_id = simple_env:get_binary("FLOKK_ADMIN_CLIENT_ID", <<"flokk-admin">>),
  flokk_admin_client_secret = simple_env:get_binary("FLOKK_ADMIN_CLIENT_SECRET"),
  flokk_admin_client_scopes = binary:split(simple_env:get_binary("FLOKK_ADMIN_CLIENT_SCOPES", <<>>), <<",">>, [global])
}).

%% Bucket name.
-define(BUCKET, <<"fk_client">>).

%% API.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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

init([]) ->
  {ok, #state{}}.

handle_call(stop, _, State) ->
  {stop, normal, stopped, State};
handle_call(list, _, State) ->
  % Response = DB:list(?BUCKET),
  Response = [<<"flokk-ui">>],
  {reply, {ok, Response}, State};
handle_call({read, ID}, _, State = #state{flokk_ui_client_id = ID, flokk_ui_client_secret = Secret, flokk_ui_client_scopes = Scopes}) ->
  % Response = DB:get(?BUCKET, ID),
  Response = [
    {<<"name">>, <<"The Flokk">>},
    {<<"description">>, <<"UI">>},
    {<<"secret">>, Secret},
    {<<"redirect_uri">>, [
      <<"http://localhost:5000">>,
      <<"http://flokk-ui.dev:5000">>,
      <<"https://test.theflokk.com">>,
      <<"https://www.theflokk.com">>
    ]},
    {<<"internal">>, true},
    {<<"scopes">>, Scopes}
  ],
  {reply, {ok, Response}, State};
handle_call({read, ID}, _, State = #state{flokk_admin_client_id = ID, flokk_admin_client_secret = Secret, flokk_admin_client_scopes = Scopes}) ->
  % Response = DB:get(?BUCKET, ID),
  Response = [
    {<<"name">>, <<"The Flokk Admin">>},
    {<<"description">>, <<"Admin UI">>},
    {<<"secret">>, Secret},
    {<<"redirect_uri">>, [
      <<"http://localhost:5003">>,
      <<"http://flokk-admin.dev:5000">>,
      <<"https://admin-test.theflokk.com">>,
      <<"https://admin.theflokk.com">>
    ]},
    {<<"internal">>, true},
    {<<"scopes">>, Scopes}
  ],
  {reply, {ok, Response}, State};
handle_call({read, _}, _, State) ->
  {reply, {error, notfound}, State};
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
handle_call({find, Query}, _, State = #state{flokk_ui_client_id = UIID, flokk_admin_client_id = AdminID}) ->
  % Response = DB:get(?BUCKET, ID),
  Results = case fast_key:get(<<"id">>, Query) of
    UIID = ID ->
      [ID];
    AdminID = ID ->
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
