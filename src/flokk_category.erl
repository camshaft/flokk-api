-module (flokk_category).

%% API.
-export([start_link/0]).
-export([stop/0]).
-export([available/0]).
-export([list/0]).
-export([get/1]).
-export([add/1]).
-export([edit/2]).
-export([remove/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Bucket name.
-define(BUCKET, <<"fk_category">>).

-record(state, {
  conn = undefined :: pid()
}).

%% API.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

available() ->
  %% TODO check the database connection
  % gen_server:call(?MODULE, available).
  true.

list() ->
  gen_server:call(?MODULE, list).

get(ID) ->
  gen_server:call(?MODULE, {get, ID}).

add(Category) ->
  gen_server:call(?MODULE, {add, Category}).

edit(ID, Properties) ->
  gen_server:call(?MODULE, {edit, ID, Properties}).

remove(ID) ->
  gen_server:call(?MODULE, {remove, ID}).


%% gen_server.

init([]) ->
  %% TODO initialize the database connection
  {ok, #state{}}.

handle_call(stop, _, State) ->
  {stop, normal, stopped, State};
handle_call(list, _, State) ->
  Categories = [
    {<<"living-room">>, [
      {<<"title">>, <<"Living Room">>}
    ]},
    {<<"bathroom">>, [
      {<<"title">>, <<"Bathroom">>}
    ]},
    {<<"kitchen">>, [
      {<<"title">>, <<"Kitchen">>}
    ]},
    {<<"accessories">>, [
      {<<"title">>, <<"Accessories">>}
    ]}
  ],
  {reply, Categories, State};
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
