-include_lib("eunit/include/eunit.hrl").

-module(eecho_test).


run_test() -> ?assert(true).


%% test is_modify_operator/2
is_modify_operator_01_test() ->
    ?assert(listener:is_modify_operator(<<"hello world insert test">>, ["INSERT"])).
is_modify_operator_02_test() ->
    ?assert(listener:is_modify_operator(<<"hello world INSERT test">>, ["DELETE", "INSERT"])).
is_modify_operator_91_test() ->
    ?assertNot(listener:is_modify_operator(<<"hello world DEL test">>, [])).

%% test is_modify_operator/1
is_modify_operator_11_test() ->
    ?assert(listener:is_modify_operator(<<"hello world insert test">>)).
is_modify_operator_12_test() ->
    ?assert(listener:is_modify_operator(<<"hello world INSERT test">>)).
is_modify_operator_13_test() ->
    ?assert(listener:is_modify_operator(<<"hello world update test">>)).
is_modify_operator_14_test() ->
    ?assert(listener:is_modify_operator(<<"hello world delete test">>)).
is_modify_operator_21_test() ->
    ?assertNot(listener:is_modify_operator(<<"hello world INS test">>)).
is_modify_operator_22_test() ->
    ?assertNot(listener:is_modify_operator(<<"hello world UP test">>)).
is_modify_operator_23_test() ->
    ?assertNot(listener:is_modify_operator(<<"hello world DEL test">>)).

%% test find_operator/2
find_11_test() ->
    ?assert(listener:find_operator(<<"hello world INSERT test">>, "INSERT")).
find_12_test() ->
    ?assert(listener:find_operator(<<"hello world INSERT test">>, "insert")).
find_13_test() ->
    ?assert(listener:find_operator(<<"hello world insert test">>, "INSERT")).
find_14_test() ->
    ?assert(listener:find_operator(<<"hello world insert test">>, "insert")).
find_21_test() ->
    ?assertNot(listener:find_operator(<<"hello world INS test">>, "insert")).
