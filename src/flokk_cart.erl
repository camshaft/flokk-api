-module (flokk_cart).

%% API.
-export([start_link/1]).
-export([stop/0]).
-export([available/0]).
-export([read/1]).
-export([initialize/1]).
-export([set/3]).
-export([remove/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Bucket name.
-define(BUCKET, <<"fk_cart">>).

%% API.

start_link(DB) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, DB, []).

stop() ->
  gen_server:call(?MODULE, stop).

available() ->
  gen_server:call(?MODULE, ping) =:= pong.

read(ID) ->
  gen_server:call(?MODULE, {read, ID}).

initialize(ID) ->
  gen_server:call(?MODULE, {initialize, ID}).

set(ID, Item, Quantity) ->
  gen_server:call(?MODULE, {set, ID, Item, Quantity}).

remove(ID, Item) ->
  gen_server:call(?MODULE, {delete, ID, Item}).


%% gen_server.

init(DB) ->
  {ok, DB}.

handle_call(stop, _, DB) ->
  {stop, normal, stopped, DB};
handle_call({read, ID}, _, DB) ->
  case DB:get(?BUCKET, ID) of
    {ok, Obj} ->
      %% TODO resolve siblings
      %% TODO translate into a nice format
      Cart = DB:body(Obj),
      {reply, {ok, Cart}, DB};
    {error, _} = Error ->
      {reply, Error, DB};
    _ ->
      {reply, {error, not_found}, DB}
  end;
handle_call({initialize, ID}, _, DB) ->
  Obj = DB:new(?BUCKET, ID, []),

  case DB:put(Obj) of
    {ok, _} ->
      {reply, ok, DB};
    Other ->
      {reply, Other, DB}
  end;
handle_call({set, ID, Item, Quantity}, _, DB) ->
  %% TODO use a better crdt
  case DB:get(?BUCKET, ID) of
    {ok, Obj} ->

      Cart = dict:from_list(DB:body(Obj)),

      UpdatedCart = dict:store(Item, Quantity, Cart),

      Obj2 = DB:set_body(dict:to_list(UpdatedCart), Obj),

      case DB:put(Obj2) of
        {ok, _} ->
          {reply, ok, DB};
        Other ->
          {reply, Other, DB}
      end;
    {error, _} = Error ->
      {reply, Error, DB};
    _ ->
      {reply, {error, not_found}, DB}
  end;
handle_call({remove, ID, Item}, _, DB) ->
  %% TODO use a better crdt
  case DB:get(?BUCKET, ID) of
    {ok, Obj} ->

      Cart = dict:from_list(DB:body(Obj)),

      UpdatedCart = dict:erase(Item, Cart),

      Obj2 = DB:set_body(dict:to_list(UpdatedCart), Obj),

      case DB:put(Obj2) of
        {ok, _} ->
          {reply, ok, DB};
        Other ->
          {reply, Other, DB}
      end;
    {error, _} = Error ->
      {reply, Error, DB};
    _ ->
      {reply, {error, not_found}, DB}
  end;
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
