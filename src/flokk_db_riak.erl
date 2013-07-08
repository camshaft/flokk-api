-module(flokk_db_riak).

%% obj
-export([new/2, new/3, new/4]).
-export([body/1]).
-export([set_body/2]).
-export([binary_index/3]).
-export([integer_index/3]).

%% connection
-export([ping/0]).
-export([get/2]).
-export([get_binary_index/3]).
-export([put/1]).
-export([list_keys/1]).

%% gen_server.
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record (state, {
  conns,
  hosts
}).

%% obj

new(Bucket, Key)->
  riakc_obj:new(Bucket, Key).

new(Bucket, Key, Value)->
  Obj = riakc_obj:new(Bucket, Key),
  ?MODULE:set_body(Value, Obj).

new(Bucket, Key, Value, Indicies)->
  Obj = riakc_obj:new(Bucket, Key),
  Obj2 = ?MODULE:set_body(Value, Obj),
  set_indicies(Value, Obj2, Indicies).

set_indicies(_, Obj, [])->
  Obj;
set_indicies(Values, Obj, [{integer, Index}|Indicies])->
  case fast_key:get(Index, Values) of
    undefined ->
      set_indicies(Values, Obj, Indicies);
    Value ->
      Obj2 = integer_index(binary_to_list(Index), [Value], Obj),
      set_indicies(Values, Obj2, Indicies)
  end;
set_indicies(Values, Obj, [Index|Indicies])->
  case fast_key:get(Index, Values) of
    undefined ->
      set_indicies(Values, Obj, Indicies);
    Value ->
      Obj2 = binary_index(binary_to_list(Index), [Value], Obj),
      set_indicies(Values, Obj2, Indicies)
  end.

body(Obj)->
  case riakc_obj:get_content_type(Obj) of
    "application/x-erlang-term" ->
      try
        binary_to_term(riakc_obj:get_value(Obj))
      catch
        _:Error ->
          {error, Error}
      end;
    Ctype ->
      {error, {unknown_content_type, Ctype}}
  end.

set_body(Value, Obj)->
  riakc_obj:update_value(Obj, term_to_binary(Value, [compressed]), <<"application/x-erlang-term">>).

binary_index(Index, Value, Obj)->
  MD = riakc_obj:get_update_metadata(Obj),
  MD2 = riakc_obj:add_secondary_index(MD, [{{binary_index, Index}, Value}]),
  riakc_obj:update_metadata(Obj, MD2).

integer_index(Index, Value, Obj)->
  MD = riakc_obj:get_update_metadata(Obj),
  MD2 = riakc_obj:add_secondary_index(MD, [{{integer_index, Index}, Value}]),
  riakc_obj:update_metadata(Obj, MD2).

%% connection

ping()->
  gen_server:call(?MODULE, ping).

get(Bucket, Key)->
  gen_server:call(?MODULE, {get, Bucket, Key}).

get_binary_index(Bucket, Index, Value)->
  gen_server:call(?MODULE, {get_binary_index, Bucket, Index, Value}).

put(Obj)->
  gen_server:call(?MODULE, {put, Obj}).

list_keys(Bucket)->
  gen_server:call(?MODULE, {list_keys, Bucket}).

%% gen_server.

start_link(Hosts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Hosts, []).

%% TODO support pooling multiple connections
init([{Host, Port}|_] = Hosts) ->
  {ok, Conn} = riakc_pb_socket:start_link(Host, Port, [auto_reconnect]),
  {ok, #state{hosts = Hosts, conns = [Conn]}}.

handle_call(stop, _, State) ->
  {stop, normal, stopped, State};
handle_call({get, Bucket, Key}, _, State = #state{conns = [Conn|_]}) ->
  Result = riakc_pb_socket:get(Conn, Bucket, Key),
  {reply, Result, State};
handle_call({get_binary_index, Bucket, Index, Value}, _, State = #state{conns = [Conn|_]}) ->
  Results = riakc_pb_socket:get_index_eq(Conn, Bucket, {binary_index, binary_to_list(Index)}, Value),
  {reply, Results, State};
handle_call({put, Obj}, _, State = #state{conns = [Conn|_]}) ->
  Result = riakc_pb_socket:put(Conn, Obj),
  {reply, Result, State};
handle_call({list_keys, Bucket}, _, State = #state{conns = [Conn|_]}) ->
  Result = riakc_pb_socket:list_keys(Conn, Bucket),
  {reply, Result, State};
handle_call(ping, _, State = #state{conns = [Conn|_]}) ->
  {reply, riakc_pb_socket:ping(Conn), State};
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
