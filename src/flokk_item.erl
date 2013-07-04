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
-export([find/1]).
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

find(Query) ->
  gen_server:call(?MODULE, {find, Query}).

sale(ID) ->
  gen_server:call(?MODULE, {sale, ID}).


%% gen_server.

init(DB) ->
  {ok, DB}.

handle_call(stop, _, DB) ->
  {stop, normal, stopped, DB};
handle_call(list, _, DB) ->
  % TODO page this
  Response = DB:list_keys(?BUCKET),
  {reply, Response, DB};
handle_call({read, ID}, _, DB) ->
  case DB:get(?BUCKET, ID) of
    {ok, Obj} ->
      User = DB:body(Obj),
      {reply, {ok, User}, DB};
    {error, _} = Error ->
      {reply, Error, DB};
    _ ->
      {reply, {error, not_found}, DB}
  end;
handle_call({create, Item}, _, DB) ->
  % TODO validate the fields
  Obj = DB:new(?BUCKET, undefined, Item, [<<"category">>, <<"vendor">>]),
  case DB:put(Obj) of
    {ok, Saved} ->
      {reply, {ok, riakc_obj:key(Saved)}, DB};
    Other ->
      {reply, Other, DB}
  end;
handle_call({update, ID, Item}, _, DB) ->
  % TODO validate the fields
  Response = DB:put(?BUCKET, ID, Item),
  {reply, Response, DB};
handle_call({delete, ID}, _, DB) ->
  Response = DB:delete(?BUCKET, ID),
  {reply, Response, DB};
handle_call({find, Query}, _, DB) ->
  case find_by_index(Query, DB) of
    {error, <<"[{unknown_field_type",Type/binary>>} ->
      io:format("unknown_field_type ~p~n", [Type]),
      {reply, {ok, []}, DB};
    Other ->
      {reply, Other, DB}
  end;
handle_call({sale, _ID}, _, DB) ->
  %% TODO
  % Response = DB:get(?BUCKET, ID),
  {Mega, Sec, _Micro} = now(),
  Timestamp = Mega * 1000000 + Sec + random:uniform(3600),
  Sale = [{<<"ending">>, Timestamp}],
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

%% internal.

find_by_index([], _DB) ->
  {ok, []};
find_by_index([{_, undefined}|Query], DB) ->
  find_by_index(Query, DB);
find_by_index([{<<"id">>, ID}|_], _DB) ->
  % TODO we should probably call to make sure this exists
  {ok, [ID]};
find_by_index([{Key, Value}|_], DB) ->
  case DB:get_binary_index(?BUCKET, Key, Value) of
    {ok, {keys, Keys}} ->
      {ok, Keys};
    Error ->
      Error
  end.
