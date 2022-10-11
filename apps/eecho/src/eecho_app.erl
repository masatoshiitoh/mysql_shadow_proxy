%%%-------------------------------------------------------------------
%% @doc eecho public API
%% @end
%%%-------------------------------------------------------------------

-module(eecho_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Hello world! ~n"),
    eecho_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
