-module(flokk_item_scoreboard).

%% api
-export([top/1]).
-export([set/3]).

%% gen_server.
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record (state, {
  url
}).

%% api

top(Board) ->
  gen_server:call(?MODULE, {top_items, Board}).

set(Item, Score, Board) ->
  gen_server:cast(?MODULE, {set, Item, Score, Board}).

%% gen_server.

start_link(URL) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, URL, []).

init(URL) ->
  {ok, #state{url = URL}}.

handle_call(stop, _, State) ->
  {stop, normal, stopped, State};
handle_call(_, _, State = #state{url = undefined}) ->
  {reply, {ok, []}, State};
handle_call({top_items, Board}, _, State) ->
  {ok, Root, Client, State2} = root(State),
  {ok, Response, _Client2, State3} = submit(<<"search">>, [{<<"board">>, Board}], Root, Client, State2),
  {reply, {ok, fast_key:get(<<"top-items">>, Response)}, State3};
handle_call(_, _, State) ->
  {reply, ignore, State}.

handle_cast({set, _Item, _Score, _Board}, State) ->
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

root(State = #state{url = URL}) ->
  %% TODO cache the root
  {ok, Client} = cowboy_client:init([]),
  {ok, Client2} = cowboy_client:request(<<"GET">>, URL, Client),
  {ok, 200, _Headers, Client3} = cowboy_client:response(Client2),
  {ok, Data, Client4} = cowboy_client:response_body(Client3),
  {ok, jsx:decode(Data), Client4, State}.

submit(Rel, Params, Root, Client, State) ->
  Form = fast_key:get(Rel, Root),
  Method = fast_key:get(<<"method">>, Form, <<"GET">>),
  Action = fast_key:get(<<"action">>, Form),
  do_submit(Method, Action, Params, Client, State).

do_submit(<<"GET">>, Action, Params, Client, State) ->
  %% TODO validate the inputs
  URL = <<Action/binary, "?", (form_encode(Params, []))/binary>>,
  {ok, Client2} = cowboy_client:request(<<"GET">>, URL, [], Client),
  {ok, 200, _Headers, Client3} = cowboy_client:response(Client2),
  {ok, Data, Client4} = cowboy_client:response_body(Client3),
  {ok, jsx:decode(Data), Client4, State};
do_submit(Method, Action, Params, Client, State) ->
  %% TODO validate the inputs
  {ok, Client2} = cowboy_client:request(Method, Action, [], jsx:encode(Params), Client),
  {ok, 200, _Headers, Client3} = cowboy_client:response(Client2),
  {ok, Data, Client4} = cowboy_client:response_body(Client3),
  {ok, jsx:decode(Data), Client4, State}.

form_encode([{Key, Value}], Acc) ->
  list_to_binary([Acc, Key, <<"=">>, cowboy_http:urlencode(Value)]);
form_encode([{Key, Value}|Params], Acc) ->
  form_encode(Params, [Acc, Key, <<"=">>, cowboy_http:urlencode(Value), <<"&">>]).
