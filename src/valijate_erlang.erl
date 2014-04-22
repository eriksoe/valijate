-module(valijate_erlang).

-export([validate/2, error_to_english/1]).

-export_type([type_spec/0, validation_error/0]).

-type condition_description() :: valijate:condition_description().
-type erlang_shallow_type() :: atom | integer | float | number | binary | boolean | list | tuple.
-type predicate() :: fun((_) -> boolean()).
-type conversion_fun() :: fun((_)->{ok,_} | {error,_}).
-type field_name() :: atom().
-type proplist_field_spec() ::
        {atom(), type_spec()}
      | {opt, atom(), type_spec(), Default::_}.
-type proplist_field_extras_spec() :: ignore_rest | {keep_rest, fun(([_])-> _)}.
-type type_spec() ::
        atom | integer | float | number | binary | boolean | list | tuple | string
      | {list, type_spec()}
      | {proplist, maybe_improper_list(proplist_field_spec(), proplist_field_extras_spec())}
      | {satisfy, predicate(), condition_description()}
      | {convert, conversion_fun(), condition_description()}
      | {member, [term()]}
      | {either, [type_spec()]}
      | {pipeline, [type_spec()]}.

-type validation_error_reason() ::
        {bad_type_spec, _}
      | {wrong_type, Value::_, Found::erlang_shallow_type(), Expected::erlang_shallow_type()}
      | {unpure_list, Tail::_, Found::erlang_shallow_type()}
      | {missing_field, field_name()}
      | {superfluous_fields, [field_name()]}
        %% TODO: Add case for converter errors.
      | {does_not_satisfy, Value::_, condition_description()}.
-type validation_error() ::
        {validation_error, erlang, term_path(), validation_error_reason()}.

-type term_path() :: [binary() | integer()].

%%%========== Main validator ========================================

%%% Validate and reform an Erlang term.
%%%
-spec validate/2 :: (_Term, _Type :: type_spec()) ->
                            validation_error().

validate(Value, Type) ->
    try {ok, validate(Value, Type, [])}
    catch throw:{validation_error,_,_,_}=VErr ->
            VErr
    end.

%% Simple types:
validate(V, atom, _)    when is_atom(V)  -> V;
validate(V, number, _)  when is_number(V)  -> V;
validate(V, integer, _) when is_integer(V)  -> V;
validate(V, float, _)   when is_float(V)  -> V;
validate(V, binary, _)  when is_binary(V)  -> V;
validate(V, boolean, _) when is_boolean(V) -> V;
validate(V=null, null, _) -> V;
validate(V, string, RevPath)  ->
    case validate(V, {list,integer}) of
        {ok,R} -> R;
        {validation_error,_,_,_} ->
            type_error(string, V, RevPath)
    end;
validate(V, Spec, RevPath) when is_atom(Spec) ->
    type_error(Spec, V, RevPath);
%% Lists:
validate(V, {list, ElemType}, RevPath) ->
    if is_list(V) ->
            validate_list(V, ElemType, RevPath);
       true ->
            type_error(list, V, RevPath)
    end;
%% Objects:
%% Proplists:
validate(Fs, {proplist, FieldTypes}, RevPath) when is_list(Fs),
                                                   is_list(FieldTypes) ->
    validate_proplist(Fs, FieldTypes, RevPath);
validate(V, {proplist, FieldTypes}, RevPath) when is_list(FieldTypes) ->
    type_error(proplist, V, RevPath);
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
validate(V, {member, AllowedValues}, RevPath) ->
    case lists:member(V, AllowedValues) of
        true -> V;
        false -> validation_error(RevPath, {not_member, V, AllowedValues})
    end;
validate(V, {either, Types}, RevPath) ->
    validate_either(V, Types, RevPath);
validate(V, {pipeline, Types}, RevPath) ->
    validate_pipeline(V, Types, RevPath);
validate(_, BadSpec, RevPath) ->
    validation_error(RevPath, {bad_type_spec, BadSpec}).


%%%---------- Lists ----------------------------------------
validate_list(List, ElemType, RevPath) ->
    validate_list(List, ElemType, RevPath, 0).

validate_list([], _ElemType, _RevPath, _Idx) -> [];
validate_list([H|T], ElemType, RevPath, Idx) ->
    [validate(H, ElemType, [Idx|RevPath])
     | validate_list(T, ElemType, RevPath, Idx+1)];
validate_list(Tail, _ElemType, RevPath, _Idx) ->
    validation_error(RevPath,
                     {unpure_list, Tail, shallow_type(Tail)}).

%%%---------- Objects ----------------------------------------
validate_proplist(Fs, FieldTypes, RevPath) ->
    {FVs,RestObj} =
        validate_proplist_fields(FieldTypes, Fs, [], RevPath),
    if RestObj==[] ->
            list_to_tuple(FVs);
       true ->
            ExtraFieldNames = [N || {N,_} <- RestObj],
            validation_error(RevPath, {superfluous_fields, ExtraFieldNames})
    end.

validate_proplist_fields([FT | FTRest], Obj, Acc, RevPath) ->
    {FV, ObjRest} = validate_proplist_field(Obj, FT, RevPath),
    validate_proplist_fields(FTRest, ObjRest, [FV|Acc], RevPath);
validate_proplist_fields([], Obj, Acc, _RevPath) ->
    {lists:reverse(Acc), Obj};
validate_proplist_fields(ignore_rest, _Obj, Acc, _RevPath) ->
    {lists:reverse(Acc), []};
validate_proplist_fields({keep_rest, F}, Obj, Acc, _RevPath) when is_function(F,1) ->
    {lists:reverse(Acc, [F(Obj)]), []};
validate_proplist_fields(BadSpec, _, _, RevPath) ->
    validation_error(RevPath, {bad_type_spec, {bad_fields_spec, BadSpec}}).


validate_proplist_field(Obj, {FName, FSpec}, RevPath) when is_atom(FName) ->
    case lists:keytake(FName, 1, Obj) of
        {value, {_,V}, Rest} ->
            {validate(V, FSpec, [FName|RevPath]), Rest};
        false ->
            validation_error(RevPath, {missing_field, FName})
    end;
validate_proplist_field(Obj, {opt, FName, FSpec, Default}, RevPath)
  when is_atom(FName) ->
    case lists:keytake(FName, 1, Obj) of
        {value, {_,V}, Rest} ->
            {validate(V, FSpec, [FName|RevPath]), Rest};
        false ->
            {Default, Obj}
    end;
validate_proplist_field(Obj, {keep_rest, F}, _RevPath) ->
    {F(Obj), []}.
%% TODO: Handle typespec error

%%%---------- Disjunction ----------------------------------------
validate_either(V, [], RevPath) ->
    %% Another possibility is to report all of the sub-validation error reasons.
    validation_error(RevPath, {does_not_satisfy, V, "any of the allowed types"});
validate_either(V, [Type|Types], RevPath) ->
    case validate(V, Type) of
        {ok,Result} -> Result;
        {validation_error,_,_,_} ->
            validate_either(V, Types, RevPath)
    end.

%%%---------- Pipeline ----------------------------------------
validate_pipeline(V, [], _RevPath) ->
    V;
validate_pipeline(V, [Type|Types], RevPath) ->
    V2 = validate(V, Type, RevPath),
    validate_pipeline(V2, Types, RevPath).


%%%========== Error reporting ========================================
type_error(ExpectedType, Value, RevPath) ->
    validation_error(RevPath,
                     {wrong_type, Value, shallow_type(Value), ExpectedType}).


validation_error(RevPath, ErrorDetail) ->
    throw({validation_error,
           erlang,
           lists:reverse(RevPath),
           ErrorDetail}).

%%% Determine the (shallow) type of a JSON term.
-spec shallow_type/1 :: (_) -> erlang_shallow_type().
shallow_type(V) when is_atom(V)    -> atom;
shallow_type(V) when is_number(V)  -> number;
shallow_type(V) when is_binary(V)  -> binary;
shallow_type(V) when is_boolean(V) -> boolean;
shallow_type(V) when is_list(V)    -> list;
shallow_type(V) when is_tuple(V)   -> tuple;
shallow_type(V) when is_reference(V) -> reference;
shallow_type(V) when is_pid(V)     -> pid;
shallow_type(V) when is_bitstring(V) -> bitstring;
shallow_type(V) when is_function(V)-> function.

%%% Convert a validation-error output to a human readable description
%%% of the error.
-spec error_to_english/1 :: (validation_error()) -> iolist().
error_to_english({validation_error, erlang, Path, Details}) ->
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
            {unpure_list, _Tail, TailType} ->
                io_lib:format("List has unpure end of type ~p", [TailType]);
            {missing_field, FieldName} ->
                io_lib:format("The object is missing field \"~s\"", [FieldName]);
            {superfluous_fields, FieldNames} ->
                "The object has superfluous fields: "
                    ++ string:join([["\"", FN, "\""] || FN <- FieldNames],
                                   ", ");
            {not_member, Value, AllowedValues} ->
                io_lib:format("The value ~p is not among the allowed values ~p\n",
                              [Value, AllowedValues]);
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
             [".", P];
        is_atom(P) ->
             [".", atom_to_list(P)]
     end
     || P <- Path].
