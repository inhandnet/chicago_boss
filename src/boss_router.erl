%% Author: jgordor
%% Created: 01/04/2011
%% Description: Minimalist Router system for Chicago Boss
-module(boss_router).

%%
%% Exported Functions
%%
-export([start/0, start/1, stop/0]).
-export([reload/1, route/3, unroute/4, handle/2, get_all/1, set_controllers/2]).

%%
%% API Functions
%%

start() ->
    start([]).

start(Options) ->
    boss_router_sup:start_link(Options).

stop() ->
    ok.

reload(Pid) ->	
    gen_server:call(Pid, reload).

route(Pid, Req, Url) ->
    gen_server:call(Pid, {route, Req, Url}).

unroute(Pid, Controller, Action, Params) ->
    gen_server:call(Pid, {unroute, Controller, Action, Params}).

handle(Pid, StatusCode) ->
    gen_server:call(Pid, {handle, StatusCode}).

get_all(Pid) ->
    gen_server:call(Pid, get_all).

set_controllers(Pid, Controllers) ->
    gen_server:call(Pid, {set_controllers, Controllers}).
