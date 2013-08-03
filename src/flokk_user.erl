-module(flokk_user).

%% API.
-export([list/1]).
-export([read/2]).
-export([create/2]).
-export([update/3]).
-export([delete/2]).
-export([find/3]).
-export([add_credit_card/4]).
-export([validate/1]).

-include("flokk.hrl").

%% Bucket name.
-define(BUCKET(Env), <<"fk_user_", Env/binary>>).

-define (TWO_I, [<<"facebook">>, <<"google">>, <<"email">>]).

%% API.

list(Env) ->
  % TODO page this
  ?FLOKK_DB:keys(?BUCKET(Env)).

read(ID, Env) ->
  ?FLOKK_DB:get(?BUCKET(Env), ID).

create(User, Env) ->
  %% Verify that the email doesn't already exist
  case ?MODULE:find(<<"email">>, fast_key:get(<<"email">>, User), Env) of
    {ok, []} ->
      case ?FLOKK_DB:create(?BUCKET(Env), User, ?TWO_I) of
        {ok, ID} ->
          case flokk_cart:initialize(ID, Env) of
            {ok, _} = Res ->
              io:format("~p~n", [Res]),
              {ok, ID};
            %% TODO if we get an error here we should do something
            Error ->
              io:format("~p~n", [Error]),
              Error
          end;
        Error ->
          Error
      end;
    {ok, _} ->
      {error, email_in_use};
    Error ->
      Error
  end.

update(ID, User, Env) ->
  %% TODO check that the email doesn't already exist
  ?FLOKK_DB:update(?BUCKET(Env), ID, User, ?TWO_I).

delete(ID, Env) ->
  ?FLOKK_DB:delete(?BUCKET(Env), ID).

find(Index, Value, Env) ->
  ?FLOKK_DB:keys_by_index(?BUCKET(Env), Index, Value).

%% TODO
add_credit_card(Url, CreditCardInfo, ID, Env) ->
  case ?FLOKK_DB:get(?BUCKET(Env), ID) of
    {ok, User} ->
      Cards = fast_key:get(<<"credit_cards">>, User, []),
      UpdatedCards = fast_key:set(Url, CreditCardInfo, Cards),
      UpdatedUser = fast_key:set(<<"credit_cards">>, UpdatedCards, User),

      ?FLOKK_DB:update(?BUCKET(Env), ID, UpdatedUser, ?TWO_I);
    Error ->
      Error
  end.

% TODO
validate(_User) ->
  true.
