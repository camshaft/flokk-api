-module (flokk_category).

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
-define(BUCKET, <<"fk_category">>).

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

create(Category) ->
  gen_server:call(?MODULE, {create, Category}).

update(ID, Category) ->
  gen_server:call(?MODULE, {update, ID, Category}).

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
    {<<"living-room">>, <<"Living Room">>},
    {<<"bathroom">>, <<"Bathroom">>},
    {<<"kitchen">>, <<"Kitchen">>},
    {<<"art">>, <<"Art">>},
    {<<"accessories">>, <<"Accessories">>}
  ],
  {reply, {ok, Response}, DB};
handle_call({read, ID}, _, DB) ->
  % Response = DB:get(?BUCKET, ID),
  Response = [
    {<<"title">>, ID}
  ],
  {reply, {ok, Response}, DB};
handle_call({create, _Category}, _, DB) ->
  % Response = DB:post(?BUCKET, Category),
  Response = <<"new-category-id">>,
  {reply, {ok, Response}, DB};
handle_call({update, _ID, _Category}, _, DB) ->
  % Response = DB:put(?BUCKET, ID, Category),
  Response = ok,
  {reply, Response, DB};
handle_call({delete, _ID}, _, DB) ->
  % Response = DB:delete(?BUCKET, ID),
  {reply, ok, DB};
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
