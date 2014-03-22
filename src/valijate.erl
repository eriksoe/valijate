-module(valijate).

-export([erlang/2,
         json/2,
         %get_env/2,
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

-spec erlang/2 :: (_, erlang_spec()) ->
                          {ok,_} | valijate_erlang:validation_error().
erlang(Data, Typespec) -> valijate_erlang:validate(Data, Typespec).

-spec json/2 :: (_, json_spec()) ->
                        {ok,_} | valijate_json:validation_error().
json(Data, Typespec) -> valijate_json:validate(Data, Typespec).

-spec error_to_english/1 :: (validation_error()) -> iolist().
error_to_english({validation_error, erlang, _, _}=VErr) ->
    valijate_erlang:error_to_english(VErr);
error_to_english({validation_error, json, _, _}=VErr) ->
    valijate_json:error_to_english(VErr).

