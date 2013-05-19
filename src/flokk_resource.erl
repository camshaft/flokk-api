-module (flokk_resource).

% -behaviour(cowboy_handler).
-export([
    init/3
  ]).

% -behaviour(cowboy_rest_handler).
-export([
    rest_init/2,
    rest_terminate/3,
    resource_available/2,
    allowed_methods/2,
    is_authorized/2,
    forbidden/2,
    content_types_accepted/2,
    content_types_provided/2,
    variances/2,
    resource_exists/2,
    generate_etag/2,
    delete_resource/2,
    delete_completed/2
  ]).

-export([
    to_json/2,
    from_json/2
  ]).

-type proplist() :: list({term(), term()}).

-record (state, {
    method :: binary(),
    id :: binary(),
    secret :: proplist(),
    data :: term(),
    body :: proplist(),
    query :: proplist(),
    options :: proplist(),
    resource :: module(),
    handler :: module(),
    command :: atom()
}).

init(_, _, _) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  lager:debug("resource:init"),
  [Method] = cowboy_req:get([method],Req),
  {ID, Req} = cowboy_req:binding(id, Req),
  Resource = proplists:get_value(resource, Opts),
  Bare = proplists:get_value(bare, Opts, false),
  {Secret, Req} = cowboy_req:meta(token_secret, Req),

  Command = case {Method, ID, Bare} of
    {_, _, true} -> call;
    {<<"GET">>, undefined, _} -> list;
    {<<"GET">>, _, _} -> read;
    {<<"POST">>, undefined, _} -> create;
    {<<"POST">>, _, _} -> update;
    {<<"PUT">>, _, _} -> replace;
    {<<"DELETE">>, _, _} -> delete
  end,

  Handler = case Command of
    call -> Resource;
    _ -> list_to_existing_atom(lists:concat([atom_to_list(Resource),"_",atom_to_list(Command)]))
  end,

  {ok, Req, #state{
    method=Method,
    id=ID,
    resource=Resource,
    command=Command,
    handler=Handler,
    secret=Secret
  }}.

rest_terminate(_Reason, _Req, _State) ->
  lager:debug("resource:terminate"),
  ok.

resource_available(Req, State = #state{resource = Resource}) ->
  lager:debug("resource:resource_available"),
  {Resource:available(), Req, State}.

allowed_methods(Req, State) ->
  lager:debug("resource:allowed_methods"),
  {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>,
    <<"HEAD">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State = #state{secret = Secret}) ->
  lager:debug("resource:is_authorized"),
  case cowboy_req:parse_header(<<"authorization">>, Req) of
    {ok, {<<"bearer">>, AccessToken}, Req2} ->
      try simple_secrets:unpack(AccessToken, Secret) of
        false ->
          lager:debug("Invalid bearer ~p",[AccessToken]),
          {{false, <<"Bearer">>}, Req2, State};
        {User} ->
          UserId = proplists:get_value(<<"u">>, User),
          Req3 = cowboy_req:set_meta(user_id, UserId, Req2),

          Scopes = proplists:get_value(<<"s">>, User, []),
          Req4 = cowboy_req:set_meta(scopes, Scopes, Req3),

          {true, Req4, State};
        _ ->
          {{false, <<"Bearer">>}, Req, State}
      catch Class:Exc ->
        lager:debug("Invalid bearer ~p:~p",[Class,Exc]),
        {{false, <<"Bearer">>}, Req, State}
      end;
    _ ->
      {true, Req, State}
  end.

forbidden(Req, State = #state{handler = Handler}) ->
  lager:debug("resource:forbidden"),
  case erlang:function_exported(Handler, scope, 2) of
    true ->
      case Handler:scope(Req, State) of
        {Scope, Req2, State2} ->
          {not flokk_auth:authorize(Scope, Req2), Req2, State2};
        _ ->
          {false, Req, State}
      end;
    false ->
      {false, Req, State}
  end.

content_types_provided(Req, State) ->
  lager:debug("resource:content_types_provided"),
  {[
    {{<<"application">>, <<"json">>, []}, to_json},
    {{<<"application">>, <<"hyper+json">>, []}, to_json}
  ], Req, State}.

%% Call
to_json(Req, State = #state{command = call, handler = Handler}) ->
  lager:debug("resource:to_json"),
  format_json(Handler:body(Req, State), Handler);

%% List
to_json(Req, State = #state{command = list, handler = Handler, data = Data}) ->
  lager:debug("resource:to_json"),
  format_json(Handler:body(Data, Req, State), Handler);

%% Read
to_json(Req, State = #state{handler = Handler, id = ID, data = Data}) ->
  lager:debug("resource:to_json"),
  format_json(Handler:body(ID, Data, Req, State), Handler).

format_json({Body, Req, State}, Handler) ->
  {Url, Req} = cowboy_req:url(Req),
  JSON = jsx:encode([
    {<<"href">>, Url},
    {<<"root">>, [
      {<<"href">>, flokk_util:resolve(<<>>,Req)}
    ]}
  |Body]),
  case erlang:function_exported(Handler, ttl, 2) of
    true ->
      {Time, Req2, State2} = Handler:ttl(Req, State#state{body=Body}),
      CacheControl = <<"max-age=",(list_to_binary(integer_to_list(Time)))/binary>>,
      Req3 = cowboy_req:set_resp_header(<<"cache-control">>, CacheControl, Req2),
      {JSON, Req3, State2};
    false ->
      {JSON, Req, State}
  end.

variances(Req, State) ->
  lager:debug("resource:variances"),
  {[<<"authorization">>], Req, State}.

resource_exists(Req, State = #state{command = call}) ->
  lager:debug("resource:resource_exists:call"),
  {true, Req, State};
resource_exists(Req, State = #state{handler = Handler, command = list}) ->
  lager:debug("resource:resource_exists:list"),
  case Handler:list(Req, State) of
    {error, _, _} ->
      {false, Req, State};
    {Data, Req2, State2} ->
      {true, Req2, State2#state{data = Data}}
  end;
resource_exists(Req, State = #state{handler = Handler, command = read, id = ID}) ->
  lager:debug("resource:resource_exists:read"),
  case Handler:read(ID, Req, State) of
    {error, _, _} ->
      {false, Req, State};
    {Data, Req2, State2} ->
      {true, Req2, State2#state{data = Data}}
  end.

generate_etag(Req, State = #state{body = Body}) ->
  lager:debug("resource:generate_etag"),
  Hash = list_to_binary(integer_to_list(erlang:phash2(Body))),
  {<<$","W/",Hash/binary,$">>, Req, State}.

content_types_accepted(Req, State) ->
  lager:debug("resource:content_types_accepted"),
  {[
    {{<<"application">>, <<"json">>, []}, from_json},
    {{<<"application">>, <<"hyper+json">>, []}, from_json}
  ], Req, State}.

from_json(Req, State) ->
  lager:debug("resource:from_json"),
  {ok, Body, Req2} = cowboy_req:body(Req),
  {Body, Req2, State}.

delete_resource(Req, State) ->
  lager:debug("resource:delete_resource"),
  {ok, Req, State}.

delete_completed(Req, State) ->
  lager:debug("resource:delete_completed"),
  {ok, Req, State}.
