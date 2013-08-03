-module(flokk_util).

-export([load_dispatch/1]).

load_dispatch(App) ->
  File = priv_dir(App),
  {ok, HostConfs} = file:consult(filename:join(File, "routes.econf")),
  HostConfs.

priv_dir(Mod) ->
  {ok, App} = application:get_application(?MODULE),
  case code:priv_dir(App) of
    {error, _} ->
      Ebin = filename:dirname(code:which(Mod)),
      filename:join(filename:dirname(Ebin), "priv");
    Path -> Path
  end.
