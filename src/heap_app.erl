%%%-------------------------------------------------------------------
%% @doc heap public API
%% @end
%%%-------------------------------------------------------------------

-module(heap_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    heap_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
