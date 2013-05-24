-module (flokk_item).

%% API.
-export([start_link/1]).
-export([stop/0]).
-export([available/0]).
-export([list/0]).
-export([read/1]).
-export([create/1]).
-export([update/2]).
-export([delete/1]).
-export([sale/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Bucket name.
-define(BUCKET, <<"fk_item">>).

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

sale(ID) ->
  gen_server:call(?MODULE, {sale, ID}).


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
    {<<"title">>, <<"Lame Print">>},
    {<<"description">>, <<"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehen- derit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.">>},
    {<<"category">>, <<"accessories">>},
    {<<"vendor_id">>, <<"1234">>},
    {<<"vendor_title">>, <<"Scott n' Dave">>},
    {<<"retail">>, 4999},
    {<<"shipping">>, 299},
    {<<"thumbnail">>, <<"https://s3.amazonaws.com/flokk-images/clock.jpg">>},
    {<<"images">>, [
      <<"https://s3.amazonaws.com/flokk-images/clock.jpg">>,
      <<"https://s3.amazonaws.com/flokk-images/clock.jpg">>,
      <<"https://s3.amazonaws.com/flokk-images/clock.jpg">>,
      <<"https://s3.amazonaws.com/flokk-images/clock.jpg">>,
      <<"https://s3.amazonaws.com/flokk-images/clock.jpg">>
    ]}
  ],
  {reply, {ok, Response}, DB};
handle_call({create, _Item}, _, DB) ->
  % Response = DB:post(?BUCKET, Item),
  Response = <<"new-item-id">>,
  {reply, {ok, Response}, DB};
handle_call({update, _ID, _Item}, _, DB) ->
  % Response = DB:put(?BUCKET, ID, Item),
  Response = ok,
  {reply, Response, DB};
handle_call({delete, _ID}, _, DB) ->
  % Response = DB:delete(?BUCKET, ID),
  {reply, ok, DB};
handle_call({sale, ID}, _, DB) ->
  % Response = DB:get(?BUCKET, ID),
  {Mega, Sec, _Micro} = now(),
  Timestamp = Mega * 1000000 + Sec + random:uniform(3600),
  Sale = case ID of
    <<"3">> -> [];
    <<"5">> -> [];
    <<"12">> -> [];
    _ -> [{<<"ending">>, Timestamp}]
  end,
  {reply, {ok, Sale}, DB};
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
