-module(dbgtracer_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

start() ->
    _Res = application:ensure_all_started(dbgtracer, permanent).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    dbgtracer_sup:start_link().

stop(_State) ->
    ok.
