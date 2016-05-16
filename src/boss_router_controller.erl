-module(boss_router_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(BOSS_ROUTES_TABLE, boss_routes).
-define(BOSS_HANDLERS_TABLE, boss_handlers).
-record(boss_route, {url, application, controller, action, params = []}).
-record(boss_handler, {status_code, application, controller, action, params = []}).

-record(state, {
        application,
        controllers = [],
        routes_table_id,
        handlers_table_id
    }).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Options) ->
    BossApp = proplists:get_value(application, Options),
    Controllers = proplists:get_value(controllers, Options, []),
    RoutesTableId = ets:new(?BOSS_ROUTES_TABLE, [ordered_set, public, {keypos, 2}]),
    HandlersTableId = ets:new(?BOSS_HANDLERS_TABLE, [ordered_set, public, {keypos, 2}]),
    State = #state{ application = BossApp, routes_table_id = RoutesTableId, 
        handlers_table_id = HandlersTableId, controllers = Controllers },
    load(State),
    {ok, State}.

handle_call(reload, _From, State) ->
    ets:delete_all_objects(State#state.routes_table_id),
    ets:delete_all_objects(State#state.handlers_table_id),
    load(State),
    {reply, ok, State};
handle_call({handle, StatusCode}, _From, State) ->
    Result = case ets:lookup(State#state.handlers_table_id, StatusCode) of
        [] ->
            not_found;
        [#boss_handler{ application = App, controller = C, action = A, params = P }] ->
            ControllerModule = list_to_atom(boss_files:web_controller(App, C)),
            {Tokens, []} = boss_controller_lib:convert_params_to_tokens(P, ControllerModule, list_to_atom(A)),
            {ok, {App, C, A, Tokens}}
    end,
    {reply, Result, State};
handle_call({route, ""}, From, State) ->
    handle_call({route, "/"}, From, State);
handle_call({route, Req, Url}, _From, State) ->
%% 	io:format("Url is:~p~n", [Url]),
	Route = case ets:lookup(State#state.routes_table_id, Url) of
        [] ->
			UrlParts = string:tokens(Url, "/"),
%% 			NewList = lists:reverse(UrlParts),
			case check_url(Req, UrlParts, Url, State) of
				not_match ->
%% 					io:format("Url is ~p~n", [Url]),
					handle_undefined_route(Req, UrlParts, State);
				Result ->
%% 					io:format("check url result is ~p~n", [Result]),
					Result
			end;
        [#boss_route{ application = App, controller = C, action = A, params = P }] ->
			ControllerModule = list_to_atom(boss_files:web_controller(App, C)),
%%             {Tokens, []} =
			{Tokens, _Vars} =	% Tokens = [paramValue1, ...], _Vars = [] or [{unhandled params}, ...]
				boss_controller_lib:convert_params_to_tokens(P, ControllerModule, list_to_atom(A)),
%% 			io:format("route: tokens is ~p, variables is ~p, app is ~p~n", [Tokens, _Vars, App]),
			{ok, {App, C, A, Tokens}}
    end,
    {reply, Route, State};
handle_call({unroute, Controller, undefined, Params}, From, State) ->
    handle_call({unroute, Controller, default_action(State, Controller), Params}, From, State);
handle_call({unroute, Controller, Action, Params}, _From, State) ->
    RoutedURL = ets:foldl(fun
            (#boss_route{ application = App, controller = C, action = A, params = P } = Route, Default) 
                when App =:= State#state.application, C =:= Controller, A =:= Action ->
                case lists:keysort(1, Params) =:= lists:keysort(1, P) of
                    true ->
                        Route#boss_route.url;
                    false ->
                        Default
                end;
            (_, Default) ->
                Default
        end, undefined, State#state.routes_table_id),
    Result = case RoutedURL of
        undefined ->
            ControllerModule = list_to_atom(boss_files:web_controller(State#state.application, Controller)),
            {Tokens, Variables1} = boss_controller_lib:convert_params_to_tokens(Params, ControllerModule, list_to_atom(Action)),
%% 			io:format("unroute: tokens is ~p, variables is ~p~n", [Tokens, Variables1]),
            URL = case Tokens of
                [] ->
                    lists:concat(["/", Controller, "/", Action]);
                _ ->
                    lists:concat(["/", Controller, "/", Action |
                            lists:foldr(fun(T, Acc) -> ["/", T | Acc] end, [], Tokens)])
            end,
            QueryString = mochiweb_util:urlencode(Variables1),
            case QueryString of
                "" ->
                    URL;
                _ ->
                    URL ++ "?" ++ QueryString
            end;
        _ ->
            RoutedURL
    end,
    {reply, Result, State};
handle_call(get_all, _From, State) ->
    Res = lists:map(fun(#boss_route{ url = U, application = App, controller = C, action = A, params = P }) -> 
                [{url, U}, {application, App}, {controller, C}, {action, A}, {params, P}]
        end, lists:flatten(ets:match(State#state.routes_table_id, '$1'))),
    {reply, Res, State};
handle_call({set_controllers, ControllerList}, _From, State) ->
    {reply, ok, State#state{ controllers = ControllerList }}.

handle_cast(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ets:delete(State#state.routes_table_id),
    ets:delete(State#state.handlers_table_id).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

check_url(_Req, UrlParts, _Url, _State) when length(UrlParts) =:= 1 ->
	not_match;
check_url(_Req, [], _Url, _State) ->
	not_match;
check_url(Req, UrlParts2, Url, State) ->
	%% url = /api2/users/2543543850943
	UrlPartsListSize = erlang:length(UrlParts2),
	{UrlParts, _RestList} = lists:split(UrlPartsListSize - 1, UrlParts2),
	UrlPartStr = string:join(UrlParts, "/"),
	FullUrlPartsStr = lists:concat(["/", UrlPartStr, "/"]),
%% 	/users/123456 --> /users/*
	MatchUrl = FullUrlPartsStr ++ "*",
	case ets:lookup(State#state.routes_table_id, MatchUrl) of
		[] ->
%% 			io:format("Can not find modules for url ~s~n", [MatchUrl]),
			check_url(Req, UrlParts, Url, State);
		[#boss_route{ application = App, controller = C, action = A, params = P }] ->
			if C =:= undefined ->
				   not_match;
			   A =:= undefined ->
				   Len = erlang:length(FullUrlPartsStr),
				   {_FullUrlPartsStr, ActionParamsStr} = lists:split(Len, Url),
				   [Action | PartParams] = string:tokens(ActionParamsStr, "/"),
%% 				   RestParams = Req:query_params(),
%% 				   Params = lists:concat([PartParams, RestParams]),
%% 				   io:format("check url params 1 is ~p~n", [PartParams]),
				   %% Params = {ParamsInRoute, ParamsInUriPath, ParamsInUrlParam}
				   {ok, {State#state.application, C, Action, PartParams}};
			   true ->
				   Len = erlang:length(FullUrlPartsStr),
				   {_FullUrlPartsStr, ActionParamsStr} = lists:split(Len, Url),
				   PartParams = string:tokens(ActionParamsStr, "/"),
%% 				   RestParams = Req:query_params(),
				   ControllerModule = list_to_atom(boss_files:web_controller(App, C)),
				   %% Tokens = [paramValue1, ...], _Vars = [] or [{unhandled params}, ...]
				   {Tokens, _Vars} = 
					   boss_controller_lib:convert_params_to_tokens(P, ControllerModule, list_to_atom(A)),
%% 				   io:format("route1: tokens is ~p, variables is ~p, app is ~p~n", [Tokens, _Vars, App]),
				   Params = lists:concat([Tokens, PartParams]),
%% 				   io:format("check url params is ~p~n", [Params]),
				   {ok, {State#state.application, C, A, Params}}
			end
	end.
	
handle_undefined_route(Req, UrlParts, State) ->
	case UrlParts of
                [Controller] -> 
%% 					io:format("controller:~p~n", [Controller]),
                    case is_controller(State, Controller) of
                        true -> {ok, {State#state.application, Controller, default_action(State, Controller), []}};
                        false -> not_found
                    end;
                [Controller, Action|PartParams] ->
%% 					io:format("controller:~p, Action:~p, Params:~p~n", [Controller, Action, PartParams]),
					case is_controller(State, Controller) of
                        true ->
%% 							RestParams = Req:query_params(),
							%% Params2 = [$id, $name] ++ [{age, 12}]
%% 							Params = lists:append(PartParams, RestParams),
%% 							io:format("handle undefined route params is ~p~n", [PartParams]),
							{ok, {State#state.application, Controller, Action, PartParams}};
						%% 							Index = string:str(ActionStr, "?"),
%% 							if Index =:= 0 ->
%% 								   {ok, {State#state.application, Controller, ActionStr, []}};
%% 							   true ->
%% 								   %% several '?' appears possiblely. eg. /state?name=Brown?White
%% 								   Action = string:left(ActionStr, Index - 1),
%% 								   ParamStr = string:substr(ActionStr, Index + 1),
%% 								   ParamValueList = string:tokens(ParamStr, "&"),
%% 								   Params = lists:foldl(
%% 											  fun (ParamValue, Acc) ->
%% 											   %% several '=' appears possiblely. eg. name=LiLei=LiHua
%% 													   Index1 = string:str(ParamValue, "="),
%% 													   if Index1 =:= 0 ->
%% 															  Acc;
%% 														  true ->
%% 															  Key = string:left(ParamValue, Index1 - 1),
%% 															  Value = string:substr(ParamValue, Index1 + 1),
%% 															  Acc ++ [{Key, Value}]
%% 													   end
%% 											  end, [], ParamValueList),
%% 								   {ok, {State#state.application, Controller, Action, Params}}
%% 							end;
                        false -> not_found
                    end;
                _ ->
                    not_found
            end.

load(State) ->
    RoutesFile = boss_files:routes_file(State#state.application),
    error_logger:info_msg("Loading routes from ~p ....~n", [RoutesFile]),
    case file:consult(RoutesFile) of
        {ok, Routes} -> 
            lists:map(fun
                    ({UrlOrStatusCode, Proplist}) when is_list(Proplist) ->
                        TheApplication = proplists:get_value(application, Proplist, 
															 State#state.application),
                        TheController = proplists:get_value(controller, Proplist),
                        TheAction = proplists:get_value(action, Proplist),
                        CleanParams = lists:foldl(fun(Key, Vars) ->
                                    proplists:delete(Key, Vars)
                            end, Proplist, [application, controller, action]),
                        case UrlOrStatusCode of
                            Url when is_list(Url) ->
                                NewRoute = #boss_route{ 
                                    url = Url, 
                                    application = TheApplication, 
                                    controller = TheController, 
                                    action = TheAction, 
                                    params = CleanParams },
                                true = ets:insert(State#state.routes_table_id, NewRoute);
                            StatusCode when is_integer(StatusCode) ->
                                NewHandler = #boss_handler{ 
                                    status_code = StatusCode, 
                                    application = TheApplication,
                                    controller = TheController,
                                    action = TheAction, 
                                    params = CleanParams },
                                true = ets:insert(State#state.handlers_table_id, NewHandler)
                        end
                end, Routes);
        Error -> 
            error_logger:error_msg("Missing or invalid boss.routes file in ~p~n~p~n", [RoutesFile, Error])
    end.

is_controller(State, Controller) -> 
    lists:member(boss_files:web_controller(State#state.application, Controller), State#state.controllers).

default_action(State, Controller) ->
    case is_controller(State, Controller) of
        true ->
            ControllerModule = list_to_atom(boss_files:web_controller(State#state.application, Controller)),
            case proplists:get_value(default_action, ControllerModule:module_info(attributes)) of
                [DefaultAction] when is_atom(DefaultAction) ->
                    atom_to_list(DefaultAction);
                _ ->
                    "index"
            end;
        false ->
            "index"
    end.

