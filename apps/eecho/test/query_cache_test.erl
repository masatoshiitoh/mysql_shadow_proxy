-include_lib("eunit/include/eunit.hrl").

-module(query_cache_test).

-define(PDIC_KEY_NAME, temporary_query_cache_key).

run_test() -> ?assert(true).

store_key_test() ->
    query_cache:store_key(<<"1234">>),
    ?assert(get(?PDIC_KEY_NAME) == <<"1234">>).


%% store_valueの戻り値の確認
store_value_test() ->
    query_cache:start_link(),
    query_cache:store_key(<<"1234">>),
    Result = query_cache:store_value(<<"vvvv">>),
    query_cache:stop(1),
    ?assert(Result == {<<"1234">>, <<"vvvv">>}).

%% 登録が期待通りの場合のlookup動作    
store_value2_test() ->
        query_cache:start_link(),
        query_cache:store_key(<<"1234">>),
        _R1 = query_cache:store_value(<<"vvvv">>),
        Result = query_cache:lookup(<<"1234">>),
        query_cache:stop(1),
        ?assert(Result == <<"vvvv">>).
    
%% キーが上書きされてしまったあとのバリュー保管、という場合のlookup動作（キーがなくてundefinedを返す）
store_value3_test() ->
        query_cache:start_link(),
        query_cache:store_key(<<"1234">>),
        query_cache:store_key(<<"2345">>),
        _R1 = query_cache:store_value(<<"vvvv">>),
        Result = query_cache:lookup(<<"1234">>),
        query_cache:stop(1),
        ?assert(Result == undefined).
    
