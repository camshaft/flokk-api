-module(flokk_resource).

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
  content_types_accepted/2,
  content_types_provided/2,
  variances/2,
  resource_exists/2,
  generate_etag/2,
  delete_resource/2
]).

-export([
  to_json/2,
  from_json/2
]).

-type proplist() :: list({term(), term()}).

-record (state, {
  method :: binary(),
  id :: binary(),
  data :: term(),
  body :: proplist(),
  query :: proplist(),
  options :: proplist(),
  resource :: module(),
  handler :: module(),
  handler_state :: any(),
  command :: atom()
}).

init(_, _, _) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  lager:debug("resource:init"),
  [Method] = cowboy_req:get([method],Req),
  {ID, Req} = cowboy_req:binding(id, Req),
  Resource = proplists:get_value(resource, Opts),

  ForcedCommand = fast_key:get(command, Opts),

  Command = case ForcedCommand of
    undefined ->
      case {Method, ID} of
        {<<"GET">>, undefined} -> list;
        {<<"GET">>, _} -> read;
        {<<"POST">>, undefined} -> create;
        {<<"POST">>, _} -> action;
        {<<"PUT">>, _} -> update;
        {<<"DELETE">>, _} -> delete
      end;
    Type ->
      Type
  end,

  Handler = case ForcedCommand of
    undefined -> list_to_atom(lists:concat([atom_to_list(Resource),"_",atom_to_list(Command)]));
    _ -> Resource
  end,

  case Handler:init(Req, Opts) of
    {ok, Req2, HandlerState} ->
      {ok, Req2, #state{
        method=Method,
        id=ID,
        resource=Resource,
        command=Command,
        handler=Handler,
        handler_state=HandlerState
      }};
    _ = Error ->
      Error
  end.

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

is_authorized(Req, State = #state{handler = Handler}) ->
  lager:debug("resource:is_authorized"),
  case cowboy_resource_owner:failed_authentication(Req) of
    true ->
      {{false, <<"Bearer">>}, Req, State};
    _ ->
      case erlang:function_exported(Handler, scope, 2) of
        true ->
          case Handler:scope(Req, State) of
            {Scope, Req2, State2} ->
              case cowboy_resource_owner:is_authorized(Scope, Req2) of
                true ->
                  {true, Req2, State2};
                false ->
                  {{false, <<"Bearer">>}, Req2, State}
              end;
            _ ->
              {{false, <<"Bearer">>}, Req, State}
          end;
        false ->
          {true, Req, State}
      end
  end.

content_types_provided(Req, State) ->
  lager:debug("resource:content_types_provided"),
  {[
    {{<<"application">>, <<"json">>, []}, to_json},
    {{<<"application">>, <<"hyper+json">>, []}, to_json}
  ], Req, State}.

%% Call
to_json(Req, State = #state{command = call, handler = Handler, body = undefined}) ->
  lager:debug("resource:to_json"),
  format_json(Handler:body(Req, State), Handler);
to_json(Req, State = #state{command = call, handler = Handler, body = Body}) ->
  lager:debug("resource:to_json"),
  format_json(Handler:body(Body, Req, State), Handler);

%% List
to_json(Req, State = #state{command = list, handler = Handler, data = Data}) ->
  lager:debug("resource:to_json"),
  format_json(Handler:body(Data, Req, State), Handler);

%% Read
to_json(Req, State = #state{handler = Handler, id = ID, data = Data}) ->
  lager:debug("resource:to_json"),
  format_json(Handler:body(ID, Data, Req, State), Handler).

format_json({Body, Req, State}, Handler) ->
  {Path, Req} = cowboy_req:path(Req),
  {QS, Req} = cowboy_req:qs(Req),
  URL = case QS of
    <<>> -> Path;
    QS -> <<Path/binary,"?",QS/binary>>
  end,
  UndefinedToNull = fun(undefined) -> null; (V) -> V end,
  JSON = jsx:encode([
    {<<"href">>, cowboy_base:resolve(URL, Req)},
    {<<"root">>, [
      {<<"href">>, cowboy_base:resolve(<<>>,Req)}
    ]}
  |Body], [{pre_encode, UndefinedToNull}]),
  case erlang:function_exported(Handler, ttl, 2) of
    true ->
      {Time, Req2, State2} = Handler:ttl(Req, State#state{body=Body}),
      CacheControl = <<"max-age=",(list_to_binary(integer_to_list(Time)))/binary,", must-revalidate, private">>,
      Req3 = cowboy_req:set_resp_header(<<"cache-control">>, CacheControl, Req2),
      {JSON, Req3, State2};
    false ->
      {JSON, Req, State}
  end.

variances(Req, State) ->
  lager:debug("resource:variances"),
  {[<<"authorization">>], Req, State}.

resource_exists(Req, State = #state{handler = Handler, command = Command}) when Command =:= call orelse Command =:= action ->
  lager:debug("resource:resource_exists:call"),
  case erlang:function_exported(Handler, Command, 2) of
    true ->
      case Handler:Command(Req, State) of
        {error, _, _} = Error ->
          Error;
        {false, _, _} = Error ->
          Error;
        {{ok, Body}, Req2, State2} ->
          {true, Req2, State2#state{body=Body}};
        {{error, notfound}, Req2, State2} ->
          {false, Req2, State2};
        {{error, _}, Req2, State2} ->
          %% TODO handle the error
          {false, Req2, State2}
      end;
    false ->
      {true, Req, State}
  end;
resource_exists(Req, State = #state{handler = Handler, command = list}) ->
  lager:debug("resource:resource_exists:list"),
  case Handler:list(Req, State) of
    {{ok, Data}, Req2, State2} ->
      {true, Req2, State2#state{data=Data}};
    {{error, _}, Req2, State2} ->
      %% TODO handle the error
      {false, Req2, State2};
    {error, _, _} ->
      {false, Req, State}
  end;
resource_exists(Req, State = #state{handler = Handler, command = read, id = ID}) ->
  lager:debug("resource:resource_exists:read"),
  case Handler:read(ID, Req, State) of
    {{ok, Data}, Req2, State2} ->
      {true, Req2, State2#state{data=Data}};
    {{error, notfound}, Req2, State2} ->
      {false, Req2, State2};
    {{error, _}, Req2, State2} ->
      %% TODO handle the error
      {false, Req2, State2};
    {error, _, _} ->
      {false, Req, State}
  end;
resource_exists(Req, State = #state{command = create}) ->
  lager:debug("resource:resource_exists:create"),
  {true, Req, State};
resource_exists(Req, State = #state{command = delete}) ->
  lager:debug("resource:resource_exists:delete"),
  %% DELETEs should be idempotent
  {true, Req, State};
resource_exists(Req, State = #state{command = update}) ->
  lager:debug("resource:resource_exists:update"),
  %% PUTs should be idempotent
  {true, Req, State}.

% Disable this for now until we move it after we generate the body
generate_etag(Req, State = #state{data = Data}) ->
  lager:debug("resource:generate_etag"),
  Hash = list_to_binary(integer_to_list(erlang:phash2(Data))),
  OwnerID = case cowboy_resource_owner:owner_id(Req) of
    undefined ->
      <<>>;
    Owner ->
      Owner
  end,
  {<<$","W/",Hash/binary,"-",OwnerID/binary,$">>, Req, State}.

content_types_accepted(Req, State) ->
  lager:debug("resource:content_types_accepted"),
  {[
    {{<<"application">>, <<"json">>, '*'}, from_json},
    {{<<"application">>, <<"hyper+json">>, '*'}, from_json}
  ], Req, State}.

from_json(Req, State = #state{command = create}) ->
  lager:debug("resource:from_json:create"),
  parse_json(Req, State, fun create_resource/4);

from_json(Req, State = #state{command = action}) ->
  lager:debug("resource:from_json:create"),
  parse_json(Req, State, fun action_resource/4);

from_json(Req, State = #state{command = update}) ->
  lager:debug("resource:from_json:update"),
  parse_json(Req, State, fun update_resource/4).

parse_json(Req, State = #state{handler = Handler} , Next) ->
  {ok, JSON, Req2} = cowboy_req:body(Req),

  %% TODO make streming - do we get any win with it?
  case jsx:decode(JSON) of
    {error, _} ->
      {false, Req2, State};
    {incomplete, _} ->
      {false, Req2, State};
    Body ->
      case erlang:function_exported(Handler, validate, 3) of
        true ->
          case Handler:validate(Body, Req2, State) of
            {true, Req3, State2} -> Next(Handler, Body, Req3, State2);
            Error -> Error
          end;
        false ->
          Next(Handler, Body, Req2, State)
      end
  end.

create_resource(Handler, Body, Req, State) ->
  case Handler:create(Body, Req, State) of
    {{ok, ID}, Req2, State2} ->
      {Loc, Req3, State3} = Handler:location(ID, Req2, State2),
      {{true, Loc}, Req3, State3};
    {{error, _}, Req2, State2} ->
      {false, Req2, State2};
    %% TODO handle error
    {error, _, _} = Error ->
      Error
  end.

action_resource(Handler, Body, Req, State = #state{id = ID}) ->
  Action = case fast_key:get(<<"action">>, Body) of
    undefined ->
      action;
    BinAction ->
      binary_to_atom(BinAction, utf8)
  end,
  case Handler:Action(ID, Body, Req, State) of
    {ok, Req2, State2} ->
      Req3 = cowboy_req:set_meta(pub_event, <<"update">>, Req2),
      {true, Req3, State2};
    {ResBody, Req2, State2} when is_list(ResBody) ->
      Req3 = cowboy_req:set_meta(pub_event, <<"update">>, Req2),
      case format_json({ResBody, Req3, State2}, Handler) of
        {JSON, Req4, State3} ->
          Req5 = cowboy_req:set_resp_body(JSON, Req4),
          {true, Req5, State3};
        _Err ->
          io:format("~p~n", [_Err]),
          {false, Req2, State2}
      end;
    Result ->
      Result
  end.

update_resource(Handler, Body, Req, State = #state{id = ID}) ->
  Handler:update(ID, Body, Req, State).

delete_resource(Req, State = #state{handler = Handler, id = ID}) ->
  lager:debug("resource:delete_resource"),
  case Handler:delete(ID, Req, State) of
    {ok, Req2, State2} = Response ->
      case erlang:function_exported(Handler, location, 2) of
        true ->
          case Handler:location(Req2, State2) of
            {error, _, _} = LocationResponse ->
              LocationResponse;
            {Location, Req3, State3} ->
              Req4 = cowboy_req:set_resp_header(<<"location">>, Location, Req3),
              {ok, Req4, State3}
          end;
        false ->
          Response
      end;
    Response ->
      Response
  end.
