%%
%% flokk.erl
%%
-module(flokk).

-export([start/0]).

%% API.

start() ->
  ok = inets:start(),
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(lager),
  ok = riakou:start(),
  ok = application:start(flokk).
