-module(valijate_json_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(LOG_TRACE(Body),
        try (Body)
        catch Cls:Err ->
                Trace = erlang:get_stacktrace(),
                error_logger:error_msg("Exception: ~p:~p\n** Trace: ~p\n",
                                       [Cls, Err, Trace]),
                erlang:raise(Cls,Err,Trace)
        end).

%%%========== Simple types: ========================================

simple_type_test() ->
    TVs = [{null, null},
           {number, 123.45},
           {string, <<"Some string">>},
           {boolean, true},
           {boolean, false},
           {object, {struct, []}},
           {object, {[]}},
           {array, []}],

    %% Matrix check: Value from V1, spec from T2:
    [if T2==T1 ->
             ?assertEqual({ok,V1}, valijate:json(V1, T2));
        T2/=T1 ->
             ?assertEqual({validation_error, json, [], {wrong_type, V1, T1, T2}},
                          valijate:json(V1, T2))
     end
     || {T1,V1} <- TVs,
        {T2,_V2} <- TVs,
        T2 /= object, T2 /= array % Only simple types.
    ].

%%%========== Arrays: ========================================
array_wrongbasetype_test() ->
    [?assertMatch({validation_error,json,[], {wrong_type, Value, _, array}},
                  valijate:json(Value, {array,number}))
     || Value <- value_collection(),
        not is_list(Value)].

array_test() ->
    PartOptions = [[], [12], [13.24], [[]]],
    [begin
         Value = Part1 ++ Part2 ++ Part3,
         ActualResult = valijate:json(Value, {array,number}),
         case lists:all(fun erlang:is_number/1, Value) of
             true ->
                 ?assertEqual({ok, Value}, ActualResult);
             false ->
                 ?assertMatch({validation_error,json,[_], _}, ActualResult)
         end
     end
     || Part1 <- PartOptions,
        Part2 <- PartOptions,
        Part3 <- PartOptions
    ].

%%%========== Objects: ========================================

object_happy_case_test() ->
    ?LOG_TRACE(begin
    Spec = {object, [{<<"fred">>, number},
                     {<<"george">>, string},
                     {<<"ginny">>, boolean}
                    ]},
    Object = {[{<<"george">>, <<"Hello, Harry!">>},
               {<<"fred">>, -123},
               {<<"ginny">>, true}]},
    ?assertEqual({ok, {-123, <<"Hello, Harry!">>, true}},
                 valijate:json(Object, Spec)) end).

object2_happy_case_test() ->
    ?LOG_TRACE(begin
    Spec = {object, [{<<"fred">>, number},
                     {<<"george">>, string},
                     {<<"ginny">>, boolean}
                    ]},
    Object = {struct,
              [{<<"george">>, <<"Hello, Harry!">>},
               {<<"fred">>, -123},
               {<<"ginny">>, true}]},
    ?assertEqual({ok, {-123, <<"Hello, Harry!">>, true}},
                 valijate:json(Object, Spec)) end).


object_superfluous_fields_test() ->
    Spec = {object, [{<<"a">>, number},
                     {<<"b">>, string}]},
    Object = {struct,
              [{<<"b">>, <<"string">>},
               {<<"a">>, -123},
               {<<"c">>, 0}]},
    ?assertEqual({validation_error, json, [], {superfluous_fields, [<<"c">>]}},
                 valijate:json(Object, Spec)).

object_ignore_rest_test() ->
    Spec = {object, [{<<"a">>, number},
                     {<<"b">>, string}
                     | ignore_rest]},
    Object = {struct,
              [{<<"b">>, <<"string">>},
               {<<"a">>, -123},
               {<<"c">>, 0}]},
    ?assertEqual({ok, {-123, <<"string">>}},
                 valijate:json(Object, Spec)).

object_keep_rest_test() ->
    Spec = {object, [{<<"a">>, number},
                     {<<"b">>, string}
                     | {keep_rest, fun(L) -> {tag, L} end}]},
    Object = {struct,
              [{<<"b">>, <<"string">>},
               {<<"a">>, -123},
               {<<"c">>, 0}]},
    ?assertEqual({ok, {-123, <<"string">>, {tag, [{<<"c">>, 0}]}}},
                 valijate:json(Object, Spec)).


%%%========== Member: ========================================

member_test() ->
    Allowed = [<<"this is allowed">>, 25,50,75.0, [], {struct, []}],
    Spec = {member, Allowed},
    lists:foreach(fun(V) -> ?assertEqual({ok,V}, valijate:json(V, Spec)) end,
                  Allowed ++ [25.0, 50.0, 75]),

    lists:foreach(fun(V) -> ?assertMatch({validation_error,json,[],{not_member,V,Allowed}},
                                         valijate:json(V, Spec)) end,
                  [<<"this is not allowed">>, 25.1,49,74.99, [1], {struct, [{<<"k">>,50}]}]),
    ok.

%%%========== Either: ========================================

%%% An empty 'either' rejects everything.
either0_test() ->
    lists:foreach(fun(V) ->
                          ?assertEqual({validation_error, json, [], {does_not_satisfy, V, "any of the allowed types"}},
                                       valijate:json(V, {either, []}))
                  end,
                  [atom, 1, 2.5, {a,pair}, [a,list]]).

either1_test() ->
    ?LOG_TRACE(begin
    TVs = [{number, 12345},
           {string, <<"Some string">>},
           {{array, number}, [123.45, 67, 8.9]}],

    %% Matrix check: Type from T1, input from Input:
    [begin
         Spec = {either, [T1]},
         TypeOK = InputType=:=T1,
         if TypeOK ->
                 ?assertEqual({ok,InputValue}, valijate:json(InputValue, Spec));
            not TypeOK ->
                 ?assertEqual({validation_error, json, [], {does_not_satisfy, InputValue, "any of the allowed types"}},
                              valijate:json(InputValue, Spec))
         end
     end
     || {T1,_} <- TVs,
        {InputType,InputValue} <- TVs
    ]
               end).

either2_test() ->
    ?LOG_TRACE(begin
    TVs = [{number, 12345},
           {string, <<"Some string">>},
           {{array, number}, [123.45, 67, 8.9]}],

    %% Matrix check: Type from T1 and T2, input from Input:
    [begin
         Spec = {either, [T1,T2]},
         TypeOK = InputType=:=T1 orelse InputType=:=T2,
         if TypeOK ->
                 ?assertEqual({ok,InputValue}, valijate:json(InputValue, Spec));
            not TypeOK ->
                 ?assertEqual({validation_error, json, [], {does_not_satisfy, InputValue, "any of the allowed types"}},
                              valijate:json(InputValue, Spec))
         end
     end
     || {T1,_} <- TVs,
        {T2,_} <- TVs,
        {InputType,InputValue} <- TVs
    ]
               end).

either4_test() ->
    TVs = [{number, 12345},
           {string, <<"Some string">>},
           {{array, number}, [123.45, 67, 8.9]}],
    Spec = {either, [T || {T,_} <- TVs]},
    lists:foreach(fun(V) ->
                          ?assertEqual({ok,V}, valijate:json(V, Spec))
                  end,
                  [V || {_,V} <- TVs]).

%%%========== Pipeline: ========================================

%%% An empty 'pipeline' accepts anything and is the identity transform.
pipeline0_test() ->
    Values = value_collection(),
    Spec = {pipeline, []},
    lists:foreach(fun(V) ->
                          ?assertEqual({ok,V}, valijate:json(V, Spec))
                  end,
                  Values).

%%% A singleton pipeline is identical to just its single type.
pipeline_singleton_test() ->
    [?assertEqual(valijate:json(Value, Type),
                  valijate:json(Value, {pipeline, [Type]}))
     || Type <- type_collection(),
        Value <- value_collection()].

%%% An identity transform in a pipeline changes nothing.
pipeline_add_identity_test() ->
    Identity = {convert, fun(X) -> {ok,X} end, "identity"},
    [begin
         ?assertEqual(valijate:json(Value, Type),
                      valijate:json(Value, {pipeline, [Type, Identity]})),
         ?assertEqual(valijate:json(Value, Type),
                      valijate:json(Value, {pipeline, [Identity, Type]}))
     end
     || Type <- type_collection(),
        Value <- value_collection()].

%%% The transformations in the pipeline are in fact applied.
pipeline_squash_test() ->
    ?LOG_TRACE(begin
    Squash  = {convert, fun(_) -> {ok, <<"squashed">>} end, "squash"},
    Squash2 = {convert, fun(_) -> {ok, <<"squashed2">>} end, "squash"},
    [begin
         ?assertEqual({ok, <<"squashed">>},
                      valijate:json(Value, {pipeline, [Squash, string]})),
         ?assertEqual({ok, <<"squashed2">>},
                      valijate:json(Value, {pipeline, [Squash, string, Squash2]})),
         Expected = case valijate:json(Value, Type) of
                        {ok,_} -> {ok, <<"squashed">>};
                        {validation_error,_,_,_}=VErr -> VErr
                    end,
         ?assertEqual(Expected,
                      valijate:json(Value, {pipeline, [Type, Squash]}))
     end
     || Type <- type_collection(),
        Value <- value_collection()]
               end).

%%%============================================================

type_collection() ->
    [number, string, boolean,
     {array,number},
     {array,string},
     {object, []},
     {object, [{<<"key">>, string}]},
     {object, [{<<"key">>, string}, {<<"otherkey">>, number}]}
    ].

value_collection() ->
    [12345, 123.45, <<"Some string">>,
     true, false,
     [],
     [123, 456, 7.89],
     [<<"a">>, <<"456">>],
     {[]},
     {struct, []},
     {struct, [{<<"key">>, <<"value">>}]},
     {struct, [{<<"otherkey">>, 98.765}, {<<"key">>, <<"value">>}]}
     ].
