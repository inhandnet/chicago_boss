%%% -------------------------------------------------------------------
%%% Author  : liuyinghui
%%% Description :
%%%
%%% Created : 2012-2-3
%%% -------------------------------------------------------------------
-module(web_controller).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("boss_web.hrl").
%% -include("dn.hrl").
-include_lib("dn/include/dn.hrl").
-include_lib("dn/include/dn_log.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, get_env/3, handle_request/3, find_application_for_path/2,
	process_request/3, process_result/2, execute_action/3, load_and_execute/3,
	process_action_result/4, process_location/3, process_redirect/3,
	render_view/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	applications = [],
	http_pid
}).


%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	gen_server:start_link({local, boss_web}, ?MODULE, [], []).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	%% if app count > 1, we should execute load_app_envs() several times.
	AppAtom = boss_files:load_app_envs(),
	Ip = get_env(AppAtom, mochiweb_ip, "0.0.0.0"),
	case dn_config:get(?PARAM_NAME_PORT) of
		undefined ->
			Port = get_env(AppAtom, port, "8088");
		Port ->
			ok
	end,
	Config = [{ip, Ip}, {port, Port}],
	%% only support one application currently
	Applications = get_env(AppAtom, applications, []),
%% 	?DEBUG("applications:~p~n", [Applications]),
	AppInfoList =
		lists:map(fun(AppName) ->
%% 						  application:start(AppName),	% application has been started, one app! self
			%% AppName is atom type
			BaseURL = get_env(AppName, base_url, "/"),
			ControllerList = boss_files:web_controller_list(AppName),
			{ok, RouterSupPid} = boss_router:start([{application, AppName},
				{controllers, ControllerList}]),
			%% atom_to_list? -> handle result from boss_router_contrller is list,
			%% for match usage in process_request, this appname must be list.
			#boss_app_info{application = AppName,
			router_sup_pid = RouterSupPid,
			base_url = (if BaseURL =:= "/" -> ""; true -> BaseURL end),
			controller_modules = ControllerList
			}
		end, Applications),
	ServerConfig =
		[{loop, fun(Req) ->
			web_controller:handle_request(Req, mochiweb_request_bridge, mochiweb_response_bridge)
		end} | Config],
%% 	?DEBUG("app info list:~p~n", [AppInfoList]),
	Pid = mochiweb_http:start(ServerConfig),
	{ok, #state{applications = AppInfoList, http_pid = Pid}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(reload_routes, _From, State) ->
	lists:map(fun(AppInfo) ->
		[{_, RouterPid, _, _}] = supervisor:which_children(AppInfo#boss_app_info.router_sup_pid),
		boss_router:reload(RouterPid)
	end, State#state.applications),
	{reply, ok, State};
handle_call(get_all_routes, _From, State) ->
	Routes = lists:map(fun(AppInfo) ->
		[{_, RouterPid, _, _}] = supervisor:which_children(AppInfo#boss_app_info.router_sup_pid),
		{AppInfo#boss_app_info.application, boss_router:get_all(RouterPid)}
	end, State#state.applications),
	{reply, Routes, State};
handle_call(get_all_application_infos, _From, #state{applications = AppInfoList} = State) ->
	{reply, AppInfoList, State};
handle_call(get_all_applications, _From, State) ->
	Applications = lists:map(fun(AppInfo) -> AppInfo#boss_app_info.application end, State#state.applications),
	{reply, Applications, State};
handle_call({router_pid, App}, _From, State) ->
	Pid = lists:foldl(fun
		(#boss_app_info{application = App1, router_sup_pid = SupPid}, _) when App1 =:= App ->
			[{_, RouterPid, _, _}] = supervisor:which_children(SupPid),
			RouterPid;
		(_, Res) ->
			Res
	end, undefined, State#state.applications),
	{reply, Pid, State};
handle_call({application_info, App}, _From, State) ->
	AppInfo = lists:keyfind(App, 2, State#state.applications),
	{reply, AppInfo, State};
handle_call({base_url, App}, _From, State) ->
	BaseURL = lists:foldl(fun
		(#boss_app_info{application = App1, base_url = URL}, _) when App1 =:= App ->
			URL;
		(_, Res) ->
			Res
	end, "", State#state.applications),
	{reply, BaseURL, State};
handle_call(Request, From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
	{noreply, State}.

get_env(AppName, Key, Default) when is_list(AppName) ->
	Application = list_to_atom(AppName),
	get_env(Application, Key, Default);

get_env(Application, Key, Default) ->
	case application:get_env(Application, Key) of
		{ok, Val} -> Val;
		_ -> Default
	end.

find_application_for_path(Path, Applications) ->
	find_application_for_path(Path, undefined, Applications, -1).

find_application_for_path(_Path, Default, [], _LongestMatch) ->
	Default;
find_application_for_path(Path, Default, [App | Rest], LongestMatch) ->
	BaseURL = boss_web:base_url(App),
	case length(BaseURL) > LongestMatch of
		true ->
			case lists:prefix(BaseURL, Path) of
				true -> find_application_for_path(Path, App, Rest, length(BaseURL));
				false -> find_application_for_path(Path, Default, Rest, LongestMatch)
			end;
		false ->
			find_application_for_path(Path, Default, Rest, LongestMatch)
	end.

handle_request(Req, RequestMod, ResponseMod) ->
	LoadedApplications = boss_web:get_all_applications(),
	Request = simple_bridge:make_request(RequestMod, Req),
%%     case Request:path() of
%% 	QueryParams = Request:query_params(),
%% 	PostParams = Request:post_params(),
%% 	io:format("query params is:~p~n", [QueryParams]),
%% 	io:format("request body is:~p~n", [PostParams]),
	case Request:path() of
		FullUrl ->
%% 			io:format("Full Url is:~p, loaded apps:~p~n", [FullUrl, LoadedApplications]),
			App = find_application_for_path(Request:path(), LoadedApplications),
			DocRoot = boss_files:static_path(App),
			case App of
				undefined ->
%% 					io:format("404 ~p not found~n", [FullUrl]),
					Response = simple_bridge:make_response(ResponseMod, {Req, DocRoot}),
					Response1 = (Response:status_code(404)):data(["The requested page was not found. "]),
					Response2 = lists:foldl(fun({K, V}, Acc) ->
						Acc:header(K, V) end, Response1, [{"Content-Type", "text/html"}]),
					Response2:build_response();
				_ ->

					BaseURL = boss_web:base_url(App),
					Url = lists:nthtail(length(BaseURL), FullUrl),
%% 			io:format("Url is:~p~n", [Url]),
					case Url of
						"/" ->
							File2 = "/heartbeat_response.html",
							Response = simple_bridge:make_response(ResponseMod, {Req, DocRoot}),
							(Response:file(File2)):build_response();
%% 				"/heartbeat_response.html" = File ->
%% %%                 "/favicon.ico" = File ->
%%                     Response = simple_bridge:make_response(ResponseMod, {Req, DocRoot}),
%%                     (Response:file(File)):build_response();
%%                 "/static/"++File ->
%% %% 					io:format("~p~n", [[$/|File]]),
%%                     Response = simple_bridge:make_response(ResponseMod, {Req, DocRoot}),
%%                     (Response:file([$/|File])):build_response();
						_ ->
%% 					io:format("app is:~p~n", [App]),
							AppInfo = boss_web:application_info(App),
%% 					io:format("app info is:~p~n", [AppInfo]),
							RouterPid = boss_web:router_pid(App),
							StartTime = erlang:timestamp(),
							{StatusCode, Headers, Payload} = process_request(AppInfo#boss_app_info{router_pid = RouterPid}, Request, Url),
							Time = dn_misc:time_diff(StartTime),
							ErrorFormat = "~s ~s ~p ~pms",
							ErrorArgs = [Request:request_method(), Request:path(), StatusCode, Time div 1000],
							case StatusCode of
								500 -> error_logger:error_msg(ErrorFormat, ErrorArgs);
								404 -> error_logger:warning_msg(ErrorFormat, ErrorArgs);
								_ -> error_logger:info_msg(ErrorFormat, ErrorArgs)
							end,
							Response = simple_bridge:make_response(ResponseMod, {Req, DocRoot}),
							Response1 = (Response:status_code(StatusCode)):data(Payload),
							Response2 = lists:foldl(fun({K, V}, Acc) -> Acc:header(K, V) end, Response1, Headers),
							Response2:build_response()
					end
			end
	end.

%% process_request(AppInfo, Req, development, "/doc/"++ModelName, SessionID) ->
%%     Result = case string:chr(ModelName, $.) of
%%         0 ->
%%             case catch load_and_execute(development, {"doc", ModelName, []}, AppInfo, Req, SessionID) of
%%                 {'EXIT', Reason} ->
%%                     {error, Reason};
%%                 Ok ->
%%                     Ok
%%             end;
%%         _ ->
%%             {not_found, "File not found"}
%%     end,
%%     process_result(AppInfo, Result);
%% process_request(AppInfo, Req, Mode, Url, SessionID) ->
process_request(AppInfo, Req, Url) ->
	RouterPid = AppInfo#boss_app_info.router_pid,
	ControllerList = boss_files:web_controller_list(AppInfo#boss_app_info.application),
	boss_router:set_controllers(RouterPid, ControllerList),
%% 	io:format("app is ~p~n", [AppInfo#boss_app_info.application]),
	Location =
		case boss_router:route(RouterPid, Req, Url) of
			{ok, {Application, Controller, Action, Tokens}}
				when Application =:= AppInfo#boss_app_info.application ->
				{Controller, Action, Tokens};
			{ok, {OtherApplication, Controller, Action, Tokens}} ->
				{redirect, {OtherApplication, Controller, Action, Tokens}};
			not_found ->
%% 			io:format("new request, Url = ~p, page not found~n", [Url]),
				case boss_router:handle(RouterPid, 404) of
					{ok, {Application, Controller, Action, Tokens}} when Application =:= AppInfo#boss_app_info.application ->
						{Controller, Action, Tokens};
					{ok, {OtherApplication, Controller, Action, Tokens}} ->
						{redirect, {OtherApplication, Controller, Action, Tokens}};
					not_found ->
						undefined
				end
		end,
%% 	?DEBUG("Got request, location is ~p~n", [Location]),
	Result = case Location of
		         undefined ->
			         {not_found, ["The requested page was not found. ",
				         "Additionally, no handler was found for processing 404 errors. ",
				         "You probably want to modify ", boss_files:routes_file(AppInfo#boss_app_info.application), " to prevent errors like this one."]};
		         {redirect, _} ->
			         Location;
		         _ ->
			         case catch load_and_execute(Location, AppInfo, Req) of
				         {'EXIT', Reason} ->
					         {error, Reason};
				         Ok ->
					         Ok
			         end
	         end,
	process_result(AppInfo, Result).

process_result(_, {error, Payload}) ->
	error_logger:error_report(Payload),
	{500, [{"Content-Type", "text/html"}], "Error: <pre>" ++ io_lib:print(Payload) ++ "</pre>"};
process_result(_, {not_found, Payload}) ->
	{404, [{"Content-Type", "text/html"}], Payload};
process_result(AppInfo, {redirect, Where}) ->
	process_result(AppInfo, {redirect, Where, []});
process_result(AppInfo, {redirect, "http://" ++ Where, Headers}) ->
	process_result(AppInfo, {redirect_external, "http://" ++ Where, Headers});
process_result(AppInfo, {redirect, "https://" ++ Where, Headers}) ->
	process_result(AppInfo, {redirect_external, "https://" ++ Where, Headers});
process_result(AppInfo, {redirect, {Application, Controller, Action, Params}, Headers}) ->
	RouterPid = if
		            AppInfo#boss_app_info.application =:= Application ->
			            AppInfo#boss_app_info.router_pid;
		            true ->
			            boss_web:router_pid(list_to_atom(lists:concat([Application])))
	            end,
	URL = boss_router:unroute(RouterPid, Controller, Action, Params),
	BaseURL = boss_web:base_url(list_to_atom(lists:concat([Application]))),
	{302, [{"Location", BaseURL ++ URL}, {"Cache-Control", "no-cache"} | Headers], ""};
process_result(AppInfo, {redirect, Where, Headers}) ->
	{302, [{"Location", AppInfo#boss_app_info.base_url ++ Where}, {"Cache-Control", "no-cache"} | Headers], ""};
process_result(_, {redirect_external, Where, Headers}) ->
	{302, [{"Location", Where}, {"Cache-Control", "no-cache"} | Headers], ""};
process_result(_, {ok, Payload, Headers}) ->
	{200, [{"Content-Type", proplists:get_value("Content-Type", Headers, "text/html")}
		| proplists:delete("Content-Type", Headers)], Payload}.

load_and_execute({Controller, _, _} = Location, AppInfo, Req) ->
%%   when Mode =:= production; Mode =:= testing ->
%% 	io:format("controller is ~p~n", [Controller]),
	case lists:member(boss_files:web_controller(AppInfo#boss_app_info.application, Controller),
		AppInfo#boss_app_info.controller_modules) of
		true -> execute_action(Location, AppInfo, Req);
		false -> render_view(Location, AppInfo, Req)
	end.
%% load_and_execute(development, {"doc", ModelName, _}, AppInfo, Req, _SessionID) ->
%%     case boss_load:load_models() of
%%         {ok, ModelModules} ->
%%             case lists:member(ModelName, lists:map(fun atom_to_list/1, ModelModules)) of
%%                 true ->
%%                     Model = list_to_atom(ModelName),
%%                     {Model, Edoc} = boss_record_compiler:edoc_module(
%%                         boss_files:model_path(ModelName++".erl"), [{private, true}]),
%%                     {ok, edoc:layout(Edoc), []};
%%                 false ->
%%                     case boss_html_doc_template:render([
%%                                 {application, AppInfo#boss_app_info.application},
%%                                 {'_base_url', AppInfo#boss_app_info.base_url},
%%                                 {models, ModelModules}]) of
%%                         {ok, Payload} ->
%%                             {ok, Payload, []};
%%                         Err ->
%%                             Err
%%                     end
%%             end;
%%         {error, ErrorList} ->
%%             render_errors(ErrorList, AppInfo, Req)
%%     end;
%% load_and_execute(development, {Controller, _, _} = Location, AppInfo, Req, SessionID) ->
%%     case boss_load:load_mail_controllers() of
%%         {ok, _} ->
%%             case boss_load:load_libraries() of
%%                 {ok, _} ->
%%                     case boss_load:load_web_controllers() of
%%                         {ok, Controllers} ->
%%                             case lists:member(boss_files:web_controller(AppInfo#boss_app_info.application, Controller),
%%                                     lists:map(fun atom_to_list/1, Controllers)) of
%%                                 true ->
%%                                     case boss_load:load_models() of
%%                                         {ok, _} ->
%%                                             execute_action(Location, AppInfo, Req, SessionID);
%%                                         {error, ErrorList} ->
%%                                             render_errors(ErrorList, AppInfo, Req)
%%                                     end;
%%                                 false ->
%%                                     render_view(Location, AppInfo, Req, SessionID)
%%                             end;
%%                         {error, ErrorList} when is_list(ErrorList) ->
%%                             render_errors(ErrorList, AppInfo, Req)
%%                     end;
%%                 {error, ErrorList} ->
%%                     render_errors(ErrorList, AppInfo, Req)
%%             end;
%%         {error, ErrorList} ->
%%             render_errors(ErrorList, AppInfo, Req)
%%     end.

%% render_errors(ErrorList, AppInfo, Req) ->
%%     case boss_html_error_template:render([{errors, ErrorList}, {request, Req},
%%                 {application, AppInfo#boss_app_info.application}]) of
%%         {ok, Payload} ->
%%             {ok, Payload, []};
%%         Err ->
%%             Err
%%     end.

execute_action(Location, AppInfo, Req) ->
	execute_action(Location, AppInfo, Req, []).

execute_action({Controller, Action}, AppInfo, Req, LocationTrail) ->
	execute_action({Controller, Action, []}, AppInfo, Req, LocationTrail);
execute_action({Controller, Action, Tokens}, AppInfo, Req, LocationTrail) when is_atom(Action) ->
	execute_action({Controller, atom_to_list(Action), Tokens}, AppInfo, Req, LocationTrail);
execute_action({Controller, Action, Tokens} = Location, AppInfo, Req, LocationTrail) ->
	case lists:member(Location, LocationTrail) of
		true ->
			{error, "Circular redirect!"};
		false ->
			% do not convert a list to an atom until we are sure the controller/action
			% pair exists. this prevents a memory leak due to atom creation.
			Module = list_to_atom(boss_files:web_controller(AppInfo#boss_app_info.application, Controller)),
			ExportStrings = lists:map(
				fun({Function, Arity}) -> {atom_to_list(Function), Arity} end,
				Module:module_info(exports)),
			ControllerInstance = case proplists:get_value("new", ExportStrings) of
				                     1 ->
					                     Module:new(Req);
				                     2 ->    % SessionID is unused, never come here in coding!
%%                     Module:new(Req, SessionID)
					                     Module:new(Req, undefined)
			                     end,
			AuthInfo = case lists:member({"before_", 2}, ExportStrings) of
				           true ->
					           case ControllerInstance:before_(Action) of
						           ok ->
							           {ok, undefined};
						           OtherInfo ->
							           OtherInfo
					           end;
				           false ->
					           {ok, undefined}
			           end,
			try
				case AuthInfo of
					{ok, Info} ->
						ActionResult =
							case proplists:get_value(Action, ExportStrings) of
								3 ->
									ActionAtom = list_to_atom(Action),
									ControllerInstance:ActionAtom(Req:request_method(), Tokens);
								4 ->
									ActionAtom = list_to_atom(Action),
									ControllerInstance:ActionAtom(Req:request_method(), Tokens, Info);
								_ ->
									undefined
							end,
						Result =
							case ActionResult of
								undefined ->
									render_view(Location, AppInfo, Req, [{"_before", Info}]);
								ActionResult ->
									process_action_result({Location, Req, [Location | LocationTrail]},
										ActionResult, AppInfo, Info)
							end,
						case proplists:get_value("after_", ExportStrings) of
							3 ->
								ControllerInstance:after_(Action, Result);
							4 ->
								ControllerInstance:after_(Action, Result, Info);
							_ ->
								Result
						end;
					{redirect, Where} ->
						{redirect, process_redirect(Controller, Where, AppInfo)}
				end
			catch _Error:Why ->
				{error, Why}
			end
	end.

process_location(Controller, [{_, _} | _] = Where, AppInfo) ->
	{_, TheController, TheAction, CleanParams} = process_redirect(Controller, Where, AppInfo),
	ControllerModule = list_to_atom(boss_files:web_controller(AppInfo#boss_app_info.application, Controller)),
	ActionAtom = list_to_atom(TheAction),
	{Tokens, []} = boss_controller_lib:convert_params_to_tokens(CleanParams, ControllerModule, ActionAtom),
	{TheController, TheAction, Tokens}.

process_redirect(Controller, [{_, _} | _] = Where, AppInfo) ->
	TheApplication = proplists:get_value(application, Where, AppInfo#boss_app_info.application),
	TheController = proplists:get_value(controller, Where, Controller),
	TheAction = proplists:get_value(action, Where),
	CleanParams = lists:foldl(fun(Key, Vars) ->
		proplists:delete(Key, Vars)
	end, Where, [application, controller, action]),
	{TheApplication, TheController, TheAction, CleanParams};
process_redirect(_, Where, _) ->
	Where.

process_action_result(Info, ok, AppInfo, AuthInfo) ->
	process_action_result(Info, {ok, []}, AppInfo, AuthInfo);
process_action_result(Info, {ok, Data}, AppInfo, AuthInfo) ->
	process_action_result(Info, {ok, Data, []}, AppInfo, AuthInfo);
process_action_result({Location, Req, _}, {ok, Data, Headers}, AppInfo, AuthInfo) ->
	render_view(Location, AppInfo, Req, [{"_before", AuthInfo} | Data], Headers);

process_action_result(Info, {render_other, OtherLocation}, AppInfo, AuthInfo) ->
	process_action_result(Info, {render_other, OtherLocation, []}, AppInfo, AuthInfo);
process_action_result(Info, {render_other, OtherLocation, Data}, AppInfo, AuthInfo) ->
	process_action_result(Info, {render_other, OtherLocation, Data, []}, AppInfo, AuthInfo);
process_action_result({{Controller, _, _}, Req, _}, {render_other, OtherLocation, Data, Headers}, AppInfo, AuthInfo) ->
	render_view(process_location(Controller, OtherLocation, AppInfo),
		AppInfo, Req, [{"_before", AuthInfo} | Data], Headers);

process_action_result({{Controller, _, _}, Req, LocationTrail}, {action_other, OtherLocation}, AppInfo, _) ->
	execute_action(process_location(Controller, OtherLocation, AppInfo), AppInfo, Req, LocationTrail);

process_action_result({_, Req, LocationTrail}, not_found, AppInfo, _) ->
	case boss_router:handle(AppInfo#boss_app_info.router_pid, 404) of
		{ok, {Application, Controller, Action, Params}} when Application =:= AppInfo#boss_app_info.application ->
			execute_action({Controller, Action, Params}, AppInfo, Req, LocationTrail);
		{ok, {OtherApplication, Controller, Action, Params}} ->
			{redirect, {OtherApplication, Controller, Action, Params}};
		not_found ->
			{not_found, "The requested page was not found. Additionally, no handler was found for processing 404 errors."}
	end;

process_action_result(Info, {redirect, Where}, AppInfo, AuthInfo) ->
	process_action_result(Info, {redirect, Where, []}, AppInfo, AuthInfo);
process_action_result({{Controller, _, _}, _, _, _}, {redirect, Where, Headers}, AppInfo, _) ->
	{redirect, process_redirect(Controller, Where, AppInfo), Headers};

process_action_result(Info, {json, Data}, AppInfo, AuthInfo) ->
	process_action_result(Info, {json, Data, []}, AppInfo, AuthInfo);
process_action_result(Info, {json, Data, Headers}, AppInfo, AuthInfo) ->
%%     process_action_result(Info, {output, boss_json:encode(Data, AppInfo#boss_app_info.model_modules),
%% 	process_action_result(Info, {output, rfc4627:encode(Data),
	%% Data is a proplist type
	process_action_result(Info, {output, dn_misc:response_body(Data),
		[{"Content-Type", proplists:get_value("Content-Type", Headers, "application/json")}
			| proplists:delete("Content-Type", Headers)]}, AppInfo, AuthInfo);

process_action_result(Info, {jsonp, Callback, Data}, AppInfo, AuthInfo) ->
	process_action_result(Info, {jsonp, Callback, Data, []}, AppInfo, AuthInfo);
process_action_result(Info, {jsonp, Callback, Data, Headers}, AppInfo, AuthInfo) ->
	JsonData = boss_json:encode(Data, AppInfo#boss_app_info.model_modules),
	process_action_result(Info, {output, Callback ++ "(" ++ JsonData ++ ");",
		[{"Content-Type", proplists:get_value("Content-Type", Headers, "application/javascript")}
			| proplists:delete("Content-Type", Headers)]}, AppInfo, AuthInfo);

process_action_result(Info, {output, Payload}, AppInfo, AuthInfo) ->
	process_action_result(Info, {output, Payload, []}, AppInfo, AuthInfo);
process_action_result(_, {output, Payload, Headers}, _, _) ->
%% 	io:format("payload is ~p~n", [Payload]),
	{ok, Payload, Headers};

process_action_result(_, Else, _, _) ->
	Else.

render_view(Location, AppInfo, Req) ->
	render_view(Location, AppInfo, Req, []).

render_view(Location, AppInfo, Req, Variables) ->
	render_view(Location, AppInfo, Req, Variables, []).

render_view({Controller, Template, _}, AppInfo, Req, Variables, Headers) ->
%% 	fixme, we should return something else
	throw({'EXIT', "not found"}).
%% 	PayLoad = proplists:get_value(result, Variables),
%% 	{ok, PayLoad, Headers}.
%% 	{not_found, io_lib:format("Result:~p. Web viewing is not supported currently!", [Variables])}.
%%     ViewPath = boss_files:web_view_path(Controller, Template),
%%     LoadResult = boss_load:load_view_if_dev(AppInfo#boss_app_info.application, ViewPath, AppInfo#boss_app_info.translator_pid),
%%     BossFlash = boss_flash:get_and_clear(SessionID),
%%     case LoadResult of
%%         {ok, Module} ->
%%             {Lang, TranslationFun} = choose_translation_fun(AppInfo#boss_app_info.translator_pid,
%%                 Module:translatable_strings(), Req:header(accept_language),
%%                 proplists:get_value("Content-Language", Headers)),
%%             case Module:render(lists:merge([{"_lang", Lang},
%%                             {"_base_url", AppInfo#boss_app_info.base_url}|Variables], BossFlash),
%%                     [{translation_fun, TranslationFun}, {locale, Lang},
%%                         {custom_tags_context, [{controller, Controller},
%%                                 {application, atom_to_list(AppInfo#boss_app_info.application)},
%%                                 {action, Template},
%%                                 {router_pid, AppInfo#boss_app_info.router_pid}]}]) of
%%                 {ok, Payload} ->
%%                     {ok, Payload, Headers};
%%                 Err ->
%%                     Err
%%             end;
%%         {error, not_found} ->
%%             {not_found, io_lib:format("The requested template (~p) was not found.", [ViewPath]) };
%%         {error, {File, [{0, _Module, "Failed to read file"}]}} ->
%%             {not_found, io_lib:format("The requested template (~p) was not found.", [File]) };
%%         {error, Error}->
%%             render_errors([not_found], AppInfo, Req).
%%     end.

%% choose_translation_fun(_, _, undefined, undefined) ->
%%     DefaultLang = boss_env:get_env(assume_locale, "en"),
%%     {DefaultLang, none};
%% choose_translation_fun(TranslatorPid, Strings, AcceptLanguages, undefined) ->
%%     DefaultLang = boss_env:get_env(assume_locale, "en"),
%%     case mochiweb_util:parse_qvalues(AcceptLanguages) of
%%         invalid_qvalue_string ->
%%             {DefaultLang, none};
%%         [{Lang, _}] ->
%%             {Lang, boss_translator:fun_for(TranslatorPid, Lang)};
%%         QValues when length(QValues) > 1 ->
%%             {BestLang, BestNetQValue} = choose_language_from_qvalues(TranslatorPid, Strings, QValues),
%%             case BestNetQValue of
%%                 0.0 -> {DefaultLang, none};
%%                 _ -> {BestLang, boss_translator:fun_for(TranslatorPid, BestLang)}
%%             end
%%     end;
%% choose_translation_fun(TranslatorPid, _, _, ContentLanguage) ->
%%     {ContentLanguage, boss_translator:fun_for(TranslatorPid, ContentLanguage)}.
%%
%% choose_language_from_qvalues(TranslatorPid, Strings, QValues) ->
%%     % calculating translation coverage is costly so we start with the most preferred
%%     % languages and work our way down
%%     SortedQValues = lists:reverse(lists:keysort(2, QValues)),
%%     AssumedLocale = boss_env:get_env(assume_locale, "en"),
%%     AssumedLocaleQValue = proplists:get_value(AssumedLocale, SortedQValues, 0.0),
%%     lists:foldl(
%%         fun
%%             ({_, ThisQValue}, {BestLang, BestTranslationScore}) when BestTranslationScore >= ThisQValue ->
%%                 {BestLang, BestTranslationScore};
%%             ({ThisLang, ThisQValue}, {_, BestTranslationScore}) when ThisLang =:= AssumedLocale andalso
%%                                                                      ThisQValue > BestTranslationScore ->
%%                 {ThisLang, ThisQValue}; % translation coverage is 100%
%%             ({ThisLang, ThisQValue}, {BestLang, BestTranslationScore}) ->
%%                 TranslationCoverage = translation_coverage(Strings, ThisLang, TranslatorPid),
%%                 TranslationScore = ThisQValue * TranslationCoverage +
%%                                     AssumedLocaleQValue * (1-TranslationCoverage),
%%                 case TranslationScore > BestTranslationScore of
%%                     true -> {ThisLang, TranslationScore};
%%                     false -> {BestLang, BestTranslationScore}
%%                 end
%%         end, {"xx-bork", 0.0}, SortedQValues).
%%
%% translation_coverage([], _, _) ->
%%     0.0;
%% translation_coverage(Strings, Locale, TranslatorPid) ->
%%     case boss_translator:is_loaded(TranslatorPid, Locale) of
%%         true ->
%%             lists:foldl(fun(String, Acc) ->
%%                         case boss_translator:lookup(TranslatorPid, String, Locale) of
%%                             undefined -> Acc;
%%                             _ -> Acc + 1
%%                         end
%%                 end, 0, Strings) / length(Strings);
%%         false ->
%%             0.0
%%     end.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% return_type() ->
%% 	Accept_hdr = (Arg#arg.headers)#headers.accept,
%% 	case Accept_hdr of
%% 		undefined ->
%% 			% return default representation;
%% 			error;
%% 		"application/xml" ->
%% 			error;
%% 			% return XML representation;
%% 		"application/json" ->
%% 			error;
%% 			% return JSON representation;
%% 		_Other ->
%% 			Msg = "Accept: application/xml, application/json",
%% 			Error = "Error 406",
%% 			[{status, 406},
%% 			 {header, {content_type,"text/html"}},
%% 			 {ehtml, [{head, [], [{title, [], Error}]},
%% 			   {body, [], [{h1, [], Error},
%% 						   {p, [], Msg}]
%% 			   }
%% 					 ]
%% 			 }
%% 			]
%% 	end.

