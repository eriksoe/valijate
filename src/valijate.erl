-module(valijate).

-export([erlang/2,
         json/2,
         get_env/4,
         noisy_get_env/4,
         error_to_english/1]).

-export_type([json_spec/0,
              erlang_spec/0,
              validation_error/0,
              condition_description/0]).

-type condition_description() :: string().
-type json_spec() :: valijate_json:type_spec().
-type erlang_spec() :: valijate_erlang:type_spec().
-type validation_error() :: valijate_erlang:validation_error()
                          | valijate_json:validation_error().

%%% Validate an Erlang term against a type specification.
-spec erlang/2 :: (_, erlang_spec()) ->
                          {ok,_} | valijate_erlang:validation_error().
erlang(Data, Typespec) -> valijate_erlang:validate(Data, Typespec).

%%% Validate a JSON value against a type specification.
-spec json/2 :: (_, json_spec()) ->
                        {ok,_} | valijate_json:validation_error().
json(Data, Typespec) -> valijate_json:validate(Data, Typespec).


%%% Obtain a given Erlang application setting, validating it against a
%%% type specification.
-type required_or_optional() :: required | {default, Value::_}.
-spec get_env/4 :: (App::atom(), Key::atom(), required_or_optional(), erlang_spec()) -> _.
get_env(App, Key, Req, Spec) when is_atom(App), is_atom(Key) ->
    case application:get_env(App, Key) of
        {ok, Value} ->
            valijate_erlang:validate(Value, Spec);
        undefined ->
            handle_absent_env_key(Req, Key)
    end.

%%% Same as get_env/4, but log something suitable in case the value is maltyped.
-spec noisy_get_env/4 :: (App::atom(), Key::atom(), required_or_optional(), erlang_spec()) -> _.
noisy_get_env(App, Key, Req, Spec) when is_atom(App), is_atom(Key) ->
    case application:get_env(App, Key) of
        {ok, Value} ->
            ValidationResult = valijate_erlang:validate(Value, Spec),
            case ValidationResult of
                {validation_error, _, _, _} ->
                    error_logger:error_msg("Configuration error: error in the value for ~s:~s\n  Value: ~p\n  Problem: ~s\n",
                                           [App, Key, Value, valijate_erlang:error_to_english(ValidationResult)]);
                _ -> ok
            end,
            ValidationResult;
        undefined ->
            handle_absent_env_key(Req, Key)
    end.


handle_absent_env_key(Req, Key) ->
    case Req of
        required -> error({application_config_not_set, Key});
        {default, DefaultValue} -> DefaultValue
    end.

%%%

-spec error_to_english/1 :: (validation_error()) -> iolist().
error_to_english({validation_error, erlang, _, _}=VErr) ->
    valijate_erlang:error_to_english(VErr);
error_to_english({validation_error, json, _, _}=VErr) ->
    valijate_json:error_to_english(VErr).

