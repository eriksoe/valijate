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

string_happy_case_test() ->
    ?assertEqual({ok, "Thith ith a tetht"},
                 valijate:erlang("Thith ith a tetht", string)).

string_bad_case_test() ->
    ?assertEqual({validation_error, erlang, [], {wrong_type, [xyz], list, string}},
                 valijate:erlang([xyz], string)),
    ?assertEqual({validation_error, erlang, [], {wrong_type, "Unpure: " ++ x, list, string}},
                 valijate:erlang("Unpure: " ++ x, string)).

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
