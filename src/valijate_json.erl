-module(valijate_json).

-export([validate/2, error_to_english/1]).

-export_type([type_spec/0, validation_error/0]).

-type condition_description() :: valijate:condition_description().
-type json_shallow_type() :: null | number | string | boolean | array | object.
-type field_name() :: binary().
-type json_object_field_spec() ::
        {field_name(), type_spec()}
      | {opt, field_name(), type_spec(), Default::_}.
-type json_field_extras_spec() :: ignore_rest | {keep_rest, fun(([_])-> _)}.
-type type_spec() ::
        number | string | boolean | null
      | {array, type_spec()}
      | {object, maybe_improper_list(json_object_field_spec(), json_field_extras_spec())}
      | {satisfy, fun((_)->_), condition_description()}
      | {convert, fun((_)->_), condition_description()}
      | {either, [type_spec()]}                 % TODO: Planned
      | {pipeline, [type_spec()]}.              % TODO: Planned

-type validation_error() ::
        {bad_type_spec, _}
      | {wrong_type, Value::_, Found::json_shallow_type(), Expected::json_shallow_type()}
      | {missing_field, field_name()}
      | {superfluous_fields, [field_name()]}
        %% TODO: Add case for converter errors.
      | {does_not_satisfy, Value::_, condition_description()}.

-type json_path() :: [binary() | integer()].

%%%========== Main validator ========================================

%%% Validate and reform a JSON term (in ejson or mochijson2 form).
%%%
-spec validate/2 :: (_JSon, _Type :: type_spec()) ->
                            {ok, _} | {validation_error, json, json_path(), validation_error()}.

validate(Value, Type) ->
    try {ok, validate(Value, Type, [])}
    catch throw:{validation_error,_,_,_}=VErr ->
            VErr
    end.

%% Simple types:
validate(V, number, _)  when is_number(V)  -> V;
validate(V, string, _)  when is_binary(V)  -> V;
validate(V, boolean, _) when is_boolean(V) -> V;
validate(V=null, null, _) -> V;
validate(V, Spec, RevPath) when is_atom(Spec) ->
    type_error(Spec, V, RevPath);
%% Arrays:
validate(V, {array, ElemType}, RevPath) ->
    if is_list(V) ->
            validate_array(V, ElemType, RevPath);
       true ->
            type_error(array, V, RevPath)
    end;
%% Objects:
validate({struct, Fs}, {object, FieldTypes}, RevPath) when is_list(Fs),
                                                           is_list(FieldTypes) ->
    %% MochiJSON style object
    validate_object(Fs, FieldTypes, RevPath);
validate({Fs}, {object, FieldTypes}, RevPath) when is_list(Fs),
                                                   is_list(FieldTypes) ->
    %% EJSON style object
    validate_object(Fs, FieldTypes, RevPath);
validate(V, {object, _FieldTypes}, RevPath) ->
    type_error(object, V, RevPath);
%% Advanced type specs:
validate(V, {satisfy, F, CondDescr}, RevPath) when is_function(F,1) ->
    case F(V) of
        true -> V;
        false -> validation_error(RevPath, {does_not_satisfy, V, CondDescr})
    end;
validate(V, {convert, F, CondDescr}, RevPath) when is_function(F,1) ->
    case F(V) of
        {ok,Result} -> Result;
        {error,_Err} -> validation_error(RevPath, {does_not_satisfy, V, CondDescr})
    end;
validate(_, BadSpec, RevPath) ->
    validation_error(RevPath, {bad_type_spec, BadSpec}).


%%%---------- Arrays ----------------------------------------
validate_array(List, ElemType, RevPath) ->
    validate_array(List, ElemType, RevPath, 0).

validate_array([], _ElemType, _RevPath, _Idx) -> [];
validate_array([H|T], ElemType, RevPath, Idx) ->
    [validate(H, ElemType, [Idx|RevPath])
     | validate_array(T, ElemType, RevPath, Idx+1)].

%%%---------- Objects ----------------------------------------
validate_object(Fs, FieldTypes, RevPath) ->
    {FVs,RestObj} =
        validate_object_fields(FieldTypes, Fs, [], RevPath),
    if RestObj==[] ->
            list_to_tuple(FVs);
       true ->
            ExtraFieldNames = [N || {N,_} <- RestObj],
            validation_error(RevPath, {superfluous_fields, ExtraFieldNames})
    end.

validate_object_fields([FT | FTRest], Obj, Acc, RevPath) ->
    {FV, ObjRest} = validate_object_field(Obj, FT, RevPath),
    validate_object_fields(FTRest, ObjRest, [FV|Acc], RevPath);
validate_object_fields([], Obj, Acc, _RevPath) ->
    {lists:reverse(Acc), Obj};
validate_object_fields(ignore_rest, _Obj, Acc, _RevPath) ->
    {lists:reverse(Acc), []};
validate_object_fields({keep_rest, F}, Obj, Acc, _RevPath) when is_function(F,1) ->
    {lists:reverse(Acc, [F(Obj)]), []};
validate_object_fields(BadSpec, _, _, RevPath) ->
    validation_error(RevPath, {bad_type_spec, {bad_fields_spec, BadSpec}}).



validate_object_field(Obj, {FName, FSpec}, RevPath) when is_binary(FName) ->
    case lists:keytake(FName, 1, Obj) of
        {value, {_,V}, Rest} ->
            {validate(V, FSpec, [FName|RevPath]), Rest};
        false ->
            validation_error(RevPath, {missing_field, FName})
    end;
validate_object_field(Obj, {opt, FName, FSpec, Default}, RevPath) ->
    case lists:keytake(FName, 1, Obj) of
        {value, {_,V}, Rest} ->
            {validate(V, FSpec, [FName|RevPath]), Rest};
        false ->
            {Default, Obj}
    end;
validate_object_field(Obj, {keep_rest, F}, _RevPath) ->
    {F(Obj), []}.


%%%========== Error reporting ========================================
type_error(ExpectedType, Value, RevPath) ->
    validation_error(RevPath,
                     {wrong_type, Value, json_type(Value), ExpectedType}).


validation_error(RevPath, ErrorDetail) ->
    throw({validation_error,
           json,
           lists:reverse(RevPath),
           ErrorDetail}).

%%% Determine the (shallow) type of a JSON term.
-spec json_type/1 :: (_) -> json_shallow_type().
json_type(null)                 -> null;
json_type(V) when is_number(V)  -> number;
json_type(V) when is_binary(V)  -> string;
json_type(V) when is_boolean(V) -> boolean;
json_type(V) when is_list(V)    -> array;
json_type({Fs})         when is_list(Fs) -> object; % EJSON style
json_type({struct, Fs}) when is_list(Fs) -> object. % MochiJSON style


%%% Convert a validation-error output to a human readable description
%%% of the error.
-spec error_to_english/1 :: (validation_error()) -> iolist().
error_to_english({validation_error, json, Path, Details}) ->
    PathTxt = path_to_string(Path),
    DetailText =
        case Details of
            {bad_type_spec, Spec} ->
                io_lib:format("The type spec ~p is malformed", [Spec]);
            {wrong_type, Value, Found, Expected} ->
                if is_atom(Value);
                   is_number(Value) ->
                        io_lib:format("Value ~p has type ~p, but ~p was expected", [Value, Found, Expected]);
                   true ->
                        io_lib:format("Value has type ~p, but ~p was expected", [Found, Expected])
                end;
            {missing_field, FieldName} ->
                io_lib:format("The object is missing field \"~s\"", [FieldName]);
            {superfluous_fields, FieldNames} ->
                "The object has superfluous fields: "
                    ++ string:join([["\"", FN, "\""] || FN <- FieldNames],
                                   ", ");
            {does_not_satisfy, Value, CondDescr} ->
                io_lib:format("The value does not satisfy ~s: ~p\n",
                              [CondDescr, Value])
        end,
    lists:flatten(io_lib:format("At path ~s : ~s", [PathTxt, DetailText])).

path_to_string([]) -> "<root>";
path_to_string(Path) ->
    [if is_integer(P) ->
             ["[", integer_to_list(P), "]"];
        is_binary(P) ->
             [".", P]
     end
     || P <- Path].
