-module(valijate_get_env_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

happy_case_required_test() ->
    Key = '$valijate_present',
    application:set_env(stdlib, Key, indeed),
    ?assertEqual({ok,indeed},
                 valijate:get_env(stdlib, Key, required, atom)).

happy_case_optional_test() ->
    Key = '$valijate_present',
    application:set_env(stdlib, Key, indeed),
    ?assertEqual({ok,indeed},
                 valijate:get_env(stdlib, Key, {default, blah}, atom)).

required_key_absent_test() ->
    Key = '$valijate_absent',
    ?assertError({application_config_not_set, Key},
                 valijate:get_env(stdlib, Key, required, atom)).

optional_key_absent_test() ->
    Key = '$valijate_absent',
    ?assertEqual(the_fallback_value, valijate:get_env(stdlib, Key, {default, the_fallback_value}, atom)).

wrong_type_test() ->
    Key = '$valijate_present',
    application:set_env(stdlib, Key, indeed),
    ?assertMatch({validation_error, erlang, [], _},
                 valijate:get_env(stdlib, Key, required, integer)).

complex_type_test() ->
    Key = '$valijate_complex',
    application:set_env(stdlib, Key, [{this,is},
                                      {one,1},
                                      {property,[list]}]),
    Type = {proplist, [{one,integer}, {this,atom}, {property, {list, atom}}]},
    ?assertMatch({ok, {1,is,[list]}},
                 valijate:get_env(stdlib, Key, required, Type)).
