-module (flokk_watcher).

%% API.
-export([start_link/1]).
-export([stop/0]).
-export([available/0]).

-export([list/1]).
-export([user_watches/1]).
-export([watch/2]).
-export([unwatch/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Bucket name.
-define(BUCKET, <<"fk_watcher">>).

%% API.

start_link(DB) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, DB, []).

stop() ->
  gen_server:call(?MODULE, stop).

available() ->
  gen_server:call(?MODULE, ping) =:= pong.

list(Item) ->
  gen_server:call(?MODULE, {list, Item}).
  % {ok, {0, []}}.

user_watches(User) ->
  gen_server:call(?MODULE, {user_watches, User}).
  % {ok, {0, []}}.

watch(User, Item) ->
  gen_server:call(?MODULE, {action, add, User, Item}).
  % {ok, {0, [User]}}.

unwatch(User, Item) ->
  gen_server:call(?MODULE, {action, delete_any, User, Item}).
  % {ok, {0, []}}.



%% gen_server.

init(DB) ->
  {ok, DB}.

handle_call(stop, _, DB) ->
  {stop, normal, stopped, DB};
handle_call({list, Item}, _, DB) ->
  case DB:get(?BUCKET, Item) of
    {ok, Obj} ->
      List = DB:body(Obj),
      {reply, {ok, length(List), List}, DB};
    {error, _} = Error ->
      {reply, Error, DB};
    _ ->
      {reply, {error, not_found}, DB}
  end;
handle_call({action, Action, User, Item}, _, DB) ->
  %% TODO use a crdt set
  case DB:get(?BUCKET, Item) of
    {ok, Obj} ->
      List = DB:body(Obj),
      update(Action, User, List, Obj, DB);
    {error, notfound} ->
      Obj = DB:new(?BUCKET, Item),
      update(Action, User, [], Obj, DB);
    {error, _} = Error ->
      {reply, Error, DB}
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

update(Action, User, List, Obj, DB) ->
  Set = gb_sets:from_list(List),
  NewSet = gb_sets:Action(User, Set),

  NewList = gb_sets:to_list(NewSet),
  NewLength = gb_sets:size(NewSet),

  NewObj = DB:set_body(NewList, Obj),

  case DB:put(NewObj) of
    {ok, _} ->
      {reply, {ok, {NewLength, NewList}}, DB};
    ok ->
      {reply, {ok, {NewLength, NewList}}, DB};
    Other ->
      {reply, Other, DB}
  end.
