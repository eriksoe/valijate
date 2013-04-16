-module(valijate_tests).

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
             ?assertEqual({ok,V1}, valijate:validate(V1, T2));
        T2/=T1 ->
             ?assertEqual({validation_error, [], {wrong_type, V1, T1, T2}},
                          valijate:validate(V1, T2))
     end
     || {T1,V1} <- TVs,
        {T2,_V2} <- TVs,
        T2 /= object, T2 /= array % Only simple types.
    ].

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
                 valijate:validate(Object, Spec)) end).

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
                 valijate:validate(Object, Spec)) end).
