-module(boss_files).
-compile(export_all).

%% -export([root_dir/0, root_src_dir/0, root_priv_dir/1, load_app_envs/1,
%% 		 web_view_path/0, web_view_path/1, static_path/1, web_controller_list/1,
%% 		 web_controller/2, routes_file/1]).


root_dir() -> filename:absname(""). %filename:join([filename:dirname(code:which(?MODULE)), ".."]).
root_src_dir() -> "src".
root_priv_dir(App) ->
	filename:join([root_dir(), "priv"]).

load_app_envs() ->
	%% content of appname.config file: {appname1, [{key1, value1}, {...}]}, {appname2, [{...}]}
%% 	ConfigFileName = lists:concat([atom_to_list(App), ".config"]),
	Dir = filename:absname(""),
	ConfigFileName = "priv/sys.config",
	FilePath = filename:join([Dir, ConfigFileName]),
	{ok, ConfigList} = file:consult(FilePath),
	InitList = proplists:get_value(init, ConfigList),
	App = proplists:get_value(app, InitList),

	ValueList = proplists:get_value(web, ConfigList),

	[application:set_env(App, Key, Value) || {Key, Value} <- ValueList],
	App.

%% 	lists:foldl(fun ({_AppConfigName, ValueList}, _Acc) ->
%% %% 						 io:format("app:~p, valuelist:~p~n", [App, ValueList]),
%% 						 [application:set_env(App, Key, Value) || {Key, Value} <- ValueList]
%% 						 end, undefined, ConfigList).

web_view_path() ->
    filename:join([root_src_dir(), "view"]).
web_view_path(Controller) ->
    filename:join([web_view_path(), Controller]).
web_view_path(Controller, Template) -> web_view_path(Controller, Template, "html").
web_view_path(Controller, Template, Extension) ->
    filename:join([web_view_path(Controller), lists:concat([Template, ".", Extension])]).

mail_view_path() ->
    filename:join([root_src_dir(), "mail", "view"]).
mail_view_path(Template) -> mail_view_path(Template, "txt").
mail_view_path(Template, Extension) ->
    filename:join([mail_view_path(), lists:concat([Template, ".", Extension])]).

model_path() -> [filename:join([root_src_dir(), "model"])].
model_path(Model) -> filename:join([hd(model_path()), Model]).

lang_path() -> filename:join([root_dir(), "priv", "lang"]).
lang_path(App) -> filename:join([root_priv_dir(App), "lang"]).
lang_path(App, Lang) -> filename:join([lang_path(App), lists:concat(["strings.", Lang, ".po"])]).

static_path(App) -> filename:join([root_priv_dir(App), "static"]).

lib_path() -> [filename:join([root_src_dir(), "lib"])].

view_lib_path() -> filename:join([root_src_dir(), "view", "lib"]).

web_controller_path() -> [filename:join([root_src_dir(), "controller"])].

mail_controller_path() -> [filename:join([root_src_dir(), "mail"])].

test_path() -> [filename:join([root_src_dir(), "test", "functional"])].

ebin_dir() -> filename:join([root_dir(), "ebin"]).

include_dir() -> filename:join([root_dir(), "include"]).

test_list() ->
    module_list(test_path()).

model_list(AppName) ->
    lists:map(fun atom_to_list/1, web_controller:get_env(AppName, model_modules, [])).

web_controller_list(AppName) ->
    lists:map(fun atom_to_list/1, web_controller:get_env(AppName, controller_modules, [])).

web_controller(AppName, Controller) ->
    lists:concat([AppName, "_", Controller, "_controller"]).

view_file_list() ->
    ViewFiles = filelib:fold_files(filename:join([root_src_dir(), "view"]), ".*\\.(html|txt)$", true, fun(F1,Acc1) -> [F1 | Acc1] end, []),
    MailPattern = filename:join([root_src_dir(), "mail", "view", "*.{html,txt}"]),
    ViewFiles ++ filelib:wildcard(MailPattern).

init_file_list(App) ->
    lists:sort(filelib:wildcard(filename:join([root_priv_dir(App), "init", "*.erl"]))).

routes_file(App) ->
%%     filename:join([root_priv_dir(App), lists:concat([App, ".routes"])]).
	filename:join([root_priv_dir(App), "sys.routes"]).

language_list() ->
    language_list_dir(lang_path()).
language_list(App) ->
    language_list_dir(lang_path(App)).

language_list_dir(Path) ->
    {ok, Files} = file:list_dir(Path),
    lists:sort(lists:map(fun("strings."++Lang) -> filename:basename(Lang, ".po") end,
            lists:filter(fun
                    ("strings."++_Lang) -> true;
                    (_) -> false
                end, Files))).

module_list(Dirs) ->
    module_list1(Dirs, []).

module_list1([], ModuleAcc) ->
    lists:sort(ModuleAcc);
module_list1([Dir|Rest], ModuleAcc) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            Modules = lists:map(fun(X) -> filename:basename(X, ".erl") end,
                lists:filter(fun
                        ("."++_) ->
                            false;
                        (File) -> lists:suffix(".erl", File)
                    end, Files)),
            module_list1(Rest, Modules ++ ModuleAcc);
        _ ->
            module_list1(Rest, ModuleAcc)
    end.

dot_app_src(AppName) ->
	filename:join(["src", lists:concat([AppName, ".app.src"])]).
