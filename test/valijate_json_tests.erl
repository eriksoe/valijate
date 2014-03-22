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
