-module(flokk_item_scoreboard).

%% api
-export([top/1]).
-export([set/3]).
-export([remove/2]).

%% gen_server.
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record (state, {
  client
}).

%% api

top(Board) ->
  gen_server:call(?MODULE, {top_items, Board}).

set(Item, Score, Board) ->
  gen_server:cast(?MODULE, {set, Item, Score, Board}).

remove(Item, Board) ->
  gen_server:cast(?MODULE, {remove, Item, Board}).

%% gen_server.

start_link(URL) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, URL, []).

init(URL) ->
  {ok, C} = case URL of
    undefined ->
      eredis:start_link();
    RedisUrl ->
      {ok, {redis, Creds, Host, Port, _, []}} = http_uri:parse(RedisUrl),
      [_, Password] = binary:split(list_to_binary(Creds), <<":">>),
      eredis:start_link(Host, Port, undefined, binary_to_list(Password))
  end,
  {ok, #state{client = C}}.

handle_call(stop, _, State) ->
  {stop, normal, stopped, State};
handle_call(_, _, State = #state{client = undefined}) ->
  {reply, {ok, []}, State};
handle_call({top_items, Board}, _, State = #state{client = C}) ->
  Query = [<<"ZREVRANGE">>, Board, <<"0">>, integer_to_binary(4 - 1), <<"WITHSCORES">>],
  Res = case eredis:q(C, Query) of
    {ok, Items} ->
      group_item_score(Items, []);
    {error, _} ->
      []
  end,
  {reply, {ok, Res}, State};
handle_call(_, _, State) ->
  {reply, ignore, State}.

handle_cast({set, Item, Score, Board}, State = #state{client = C}) ->
  eredis:q(C, [<<"ZADD">>, Board, Score, Item]),
  {noreply, State};
handle_cast({set, Item, Board}, State = #state{client = C}) ->
  eredis:q(C, [<<"ZREM">>, Board, Item]),
  {noreply, State};
handle_cast(_, State) ->
  {noreply, State}.

handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% private

group_item_score([], Acc) ->
  lists:reverse(Acc);
group_item_score([Item,Score|Rest], Acc) ->
  group_item_score(Rest, [[{<<"item">>, Item}, {<<"score">>, Score}]|Acc]).
