-module(boss_controller_lib).
-export([convert_params_to_tokens/3]).

%% @doc get values for the params of methods defined in controllers. the values is defined in the 
%% static file appName.routes.
convert_params_to_tokens(Variables, ControllerModule, Action) ->
    DummyController = apply(ControllerModule, new, lists:seq(1, proplists:get_value(new, ControllerModule:module_info(exports)))), 
    Routes = DummyController:'_routes'(),
    lists:foldr(fun
            ({RouteName, RouteTokens}, {Acc, Vars}) when RouteName =:= Action ->
                Result = lists:foldr(fun
                        (_, false) ->
                            false;
                        (Token, {Acc1, Vars1}) when is_atom(Token) ->
                            CamelCase = atom_to_list(Token),
                            Underscore = list_to_atom(string:to_lower(inflector:underscore(CamelCase))),
                            case proplists:get_value(Underscore, Vars1) of
                                undefined ->
                                    false;
                                Value ->
                                    {[Value|Acc1], proplists:delete(Underscore, Vars1)}
                            end;
                        (Token, {Acc1, Vars1}) ->
                            {[Token|Acc1], Vars1}
                    end, {[], Variables}, RouteTokens),
                case Result of
                    false ->
                        {Acc, Vars};	% stop at the param, the value of which is undefined in .routes 
                    {Acc1, Vars1} ->
                        case length(Vars1) =< length(Vars) of
                            true ->
                                {Acc1, Vars1};
                            false ->
                                {Acc, Vars}
                        end
                end;
            (_, {Acc, Vars}) ->		% check the next param if no action defined for the param token
                {Acc, Vars}
        end, {[], Variables}, Routes).
