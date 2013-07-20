-module (flokk_user).

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
-export([add_credit_card/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Bucket name.
-define(BUCKET, <<"fk_user">>).

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

add_credit_card(ID, CreditCard) ->
  gen_server:call(?MODULE, {add_credit_card, ID, CreditCard}).


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
handle_call({create, User}, _, DB) ->
  Indicies = [<<"facebook">>, <<"google">>, <<"email">>],
  %% TODO validate the properties
  %% TODO make sure passhash and email are at least set
  Obj = DB:new(?BUCKET, undefined, User, Indicies),

  %% Verify that the email doesn't exist
  case find_by_index([{<<"email">>, fast_key:get(<<"email">>, User)}], DB) of
    {ok, []} ->
      case DB:put(Obj) of
        {ok, Saved} ->
          ID = riakc_obj:key(Saved),
          case flokk_cart:initialize(ID) of
            ok ->
              {reply, {ok, ID}, DB};
            Other ->
              {reply, Other, DB}
          end;
        Other ->
          {reply, Other, DB}
      end;
    %% This user already exists
    {ok, _} ->
      {reply, {error, email_in_use}, DB};
    Error ->
      {reply, Error, DB}
  end;

handle_call({update, _ID, _User}, _, DB) ->
  % Response = DB:put(?BUCKET, ID, User),
  Response = ok,
  {reply, Response, DB};
handle_call({delete, _ID}, _, DB) ->
  % Response = DB:delete(?BUCKET, ID),
  {reply, ok, DB};
handle_call({find, Query}, _, DB) ->
  case find_by_index(Query, DB) of
    {error, <<"[{unknown_field_type",Type/binary>>} ->
      io:format("unknown_field_type ~p~n", [Type]),
      {reply, {ok, []}, DB};
    Other ->
      {reply, Other, DB}
  end;
handle_call({add_credit_card, ID, CreditCard}, _, DB) ->
  case DB:get(?BUCKET, ID) of
    {ok, Obj} ->

      User = DB:body(Obj),

      Cards = fast_key:get(<<"credit_cards">>, User, []),

      %% TODO check to see if we already have the hash in the list
      UpdatedCards = [CreditCard|Cards],

      UpdatedUser = fast_key:set(<<"credit_cards">>, UpdatedCards, User),

      Obj2 = DB:set_body(UpdatedUser, Obj),

      case DB:put(Obj2) of
        {ok, _} ->
          {reply, {ok, UpdatedUser}, DB};
        ok ->
          {reply, {ok, UpdatedUser}, DB};
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
