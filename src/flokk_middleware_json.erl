-module (flokk_middleware_json).

-export([execute/2]).

%% TODO only encode if the user accepts json
%% TODO do we need to even check? how many media types do we even need to support?
execute(Req, Env) ->
  {Body, Req} = cowboy_req:meta(body, Req),
  {Status, Req} = cowboy_req:meta(status, Req, 200),
  case Body of
    undefined ->
      {ok, Req, Env};
    Body ->
      OrigPath = proplists:get_value(orig_path, Env, <<"/">>),
      Body2 = [{<<"href">>, flokk_util:resolve(OrigPath, Req)}|Body],
      {ok, Req1} = cowboy_req:reply(Status, [{<<"content-type">>, <<"application/json">>}], jsx:encode(Body2), Req),
      {ok, Req1, Env}
  end.
