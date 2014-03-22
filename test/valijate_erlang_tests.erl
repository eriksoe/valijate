-module(valijate_erlang_tests).

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
    ?LOG_TRACE(begin
    TVs = [{atom, xyz},
           {number, 123.45},
           {string, "Some string"},
           {binary, <<"Some string">>},
           {boolean, true},
           {boolean, false},
           {list, [x]},
           {tuple, {}},
           {tuple, {a,2,"c"}},
           {proplist, [{foo, "foo"}, {bar, 123.45}]},
           {proplist, []}],

    %% Matrix check: Value from V1, spec from T2:
    [begin
         TypeOK = if
                      T2=:=T1                 -> true;
                      %% Handle subcasing:
                      T2==atom,   T1==boolean -> true;
                      T2==number, T1==integer -> true;
                      T2==number, T1==float   -> true;
                      T2==list,   T1==string  -> true;
                      T2==list,   T1==proplist-> true;

                      true                    -> false
                  end,
         if TypeOK ->
                 ?assertEqual({ok,V1}, valijate:erlang(V1, T2));
            not TypeOK ->
                 TSeen = case T1 of
                             string -> list;
                             proplist -> list;
                             boolean -> atom;
                             _ -> T1
                         end,
                 ?assertEqual({validation_error, erlang, [], {wrong_type, V1, TSeen, T2}},
                              valijate:erlang(V1, T2))
         end
     end
     || {T1,V1} <- TVs,
        {T2,_V2} <- TVs,
        T2 /= list, T2 /= tuple, T2 /= string, T2 /= proplist % Only simple types.
    ]
    end).

%%%========== Lists: ========================================

list_wrongbasetype_test() ->
    [?assertMatch({validation_error,erlang,[], {wrong_type, Value, _, list}},
                  valijate:erlang(Value, {list,atom}))
     || Value <- value_collection(),
        not is_list(Value)].

list_test() ->
    PartOptions = [[], [a], [abcdefg], [1], [[]]],
    [begin
         Value = Part1 ++ Part2 ++ Part3,
         ActualResult = valijate:erlang(Value, {list,atom}),
         case lists:all(fun erlang:is_atom/1, Value) of
             true ->
                 ?assertEqual({ok, Value}, ActualResult);
             false ->
                 ?assertMatch({validation_error,erlang,[_], _}, ActualResult)
         end
     end
     || Part1 <- PartOptions,
        Part2 <- PartOptions,
        Part3 <- PartOptions
    ].

%%%========== Strings: ========================================

string_happy_case_test() ->
    ?assertEqual({ok, "Thith ith a tetht"},
                 valijate:erlang("Thith ith a tetht", string)).

string_bad_case_test() ->
    ?assertEqual({validation_error, erlang, [], {wrong_type, [xyz], list, string}},
                 valijate:erlang([xyz], string)),
    ?assertEqual({validation_error, erlang, [], {wrong_type, "Unpure: " ++ x, list, string}},
                 valijate:erlang("Unpure: " ++ x, string)).

%%%========== Property lists: ========================================

proplist_happy_case_test() ->
    ?LOG_TRACE(begin
    Spec = {proplist, [{fred,   binary},
                       {george, string},
                       {ginny,  boolean}
                      ]},
    Object = [{george, "Hello, Harry!"},
              {fred, <<"Hello, Harry!">>},
              {ginny, true}],
    ?assertEqual({ok, {<<"Hello, Harry!">>, "Hello, Harry!", true}},
                 valijate:erlang(Object, Spec)) end).


proplist_ignore_rest_test() ->
    Spec = {proplist, [{a, number},
                       {b, string}
                       | ignore_rest]},
    Proplist = [{b, "string"},
                {a, -123},
                {c, 0}],
    ?assertEqual({ok, {-123, "string"}},
                 valijate:erlang(Proplist, Spec)).

proplist_keep_rest_test() ->
    Spec = {proplist, [{a, number},
                       {b, string}
                       | {keep_rest, fun(L) -> {tag, L} end}]},
    Proplist = [{b, "string"},
                {a, -123},
                {c, 0}],
    ?assertEqual({ok, {-123, "string", {tag, [{c, 0}]}}},
                 valijate:erlang(Proplist, Spec)).

%%%========== Member: ========================================

member_test() ->
    Allowed = [a,1,{a,pair},[a,list]],
    Spec = {member, Allowed},
    lists:foreach(fun(V) -> ?assertEqual({ok,V}, valijate:erlang(V, Spec)) end,
                  [a, 1, {a,pair}, [a,list]]),

    lists:foreach(fun(V) -> ?assertMatch({validation_error,erlang,[],{not_member,V,Allowed}},
                                         valijate:erlang(V, Spec)) end,
                  [b, 12, {another,pair}, [a,list,again]]),
    ok.

%%%========== Either: ========================================

%%% An empty 'either' rejects everything.
either0_test() ->
    lists:foreach(fun(V) ->
                          ?assertEqual({validation_error, erlang, [], {does_not_satisfy, V, "any of the allowed types"}},
                                       valijate:erlang(V, {either, []}))
                  end,
                  [atom, 1, 2.5, {a,pair}, [a,list]]).

either1_test() ->
    ?LOG_TRACE(begin
    TVs = [{atom, xyz},
           {integer, 12345},
           {float, 123.45},
           {binary, <<"Some string">>}],

    %% Matrix check: Type from T1, input from Input:
    [begin
         Spec = {either, [T1]},
         TypeOK = InputType=:=T1,
         if TypeOK ->
                 ?assertEqual({ok,InputValue}, valijate:erlang(InputValue, Spec));
            not TypeOK ->
                 ?assertEqual({validation_error, erlang, [], {does_not_satisfy, InputValue, "any of the allowed types"}},
                              valijate:erlang(InputValue, Spec))
         end
     end
     || {T1,_} <- TVs,
        {InputType,InputValue} <- TVs
    ]
               end).

either2_test() ->
    ?LOG_TRACE(begin
    TVs = [{atom, xyz},
           {integer, 12345},
           {float, 123.45},
           {binary, <<"Some string">>}],

    %% Matrix check: Type from T1 and T2, input from Input:
    [begin
         Spec = {either, [T1,T2]},
         TypeOK = InputType=:=T1 orelse InputType=:=T2,
         if TypeOK ->
                 ?assertEqual({ok,InputValue}, valijate:erlang(InputValue, Spec));
            not TypeOK ->
                 ?assertEqual({validation_error, erlang, [], {does_not_satisfy, InputValue, "any of the allowed types"}},
                              valijate:erlang(InputValue, Spec))
         end
     end
     || {T1,_} <- TVs,
        {T2,_} <- TVs,
        {InputType,InputValue} <- TVs
    ]
               end).

either4_test() ->
    TVs = [{atom, xyz},
           {integer, 12345},
           {float, 123.45},
           {binary, <<"Some string">>}],
    Spec = {either, [T || {T,_} <- TVs]},
    lists:foreach(fun(V) ->
                          ?assertEqual({ok,V}, valijate:erlang(V, Spec))
                  end,
                  [V || {_,V} <- TVs]).

%%%========== Pipeline: ========================================

%%% An empty 'pipeline' accepts anything and is the identity transform.
pipeline0_test() ->
    Values = value_collection(),
    Spec = {pipeline, []},
    lists:foreach(fun(V) ->
                          ?assertEqual({ok,V}, valijate:erlang(V, Spec))
                  end,
                  Values).

%%% A singleton pipeline is identical to just its single type.
pipeline_singleton_test() ->
    [?assertEqual(valijate:erlang(Value, Type),
                  valijate:erlang(Value, {pipeline, [Type]}))
     || Type <- type_collection(),
        Value <- value_collection()].

%%% An identity transform in a pipeline changes nothing.
pipeline_add_identity_test() ->
    Identity = {convert, fun(X) -> {ok,X} end, "identity"},
    [begin
         ?assertEqual(valijate:erlang(Value, Type),
                      valijate:erlang(Value, {pipeline, [Type, Identity]})),
         ?assertEqual(valijate:erlang(Value, Type),
                      valijate:erlang(Value, {pipeline, [Identity, Type]}))
     end
     || Type <- type_collection(),
        Value <- value_collection()].

%%% The transformations in the pipeline are in fact applied.
pipeline_squash_test() ->
    Squash  = {convert, fun(X) -> {ok, squashed} end, "squash"},
    Squash2 = {convert, fun(X) -> {ok, squashed2} end, "squash"},
    [begin
         ?assertEqual({ok, squashed},
                      valijate:erlang(Value, {pipeline, [Squash, atom]})),
         ?assertEqual({ok, squashed2},
                      valijate:erlang(Value, {pipeline, [Squash, atom, Squash2]})),
         Expected = case valijate:erlang(Value, Type) of
                        {ok,_} -> {ok, squashed};
                        {validation_error,_,_,_}=VErr -> VErr
                    end,
         ?assertEqual(Expected,
                      valijate:erlang(Value, {pipeline, [Type, Squash]}))
     end
     || Type <- type_collection(),
        Value <- value_collection()].


%%%============================================================

type_collection() ->
    [atom, integer, float, number, binary,
     {list,atom},
     {list,integer},
     {proplist, [{a,atom}, {list, {list,integer}}, {property,integer}]}
    ].

value_collection() ->
    [xyz, 12345, 123.45, <<"Some string">>,
     [a,list,'of',atoms],
     [{a,xxx},{property,2},{list,[]}]
     ].
