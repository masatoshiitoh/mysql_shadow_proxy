-include_lib("eunit/include/eunit.hrl").

-module(query_cache_test).

-define(PDIC_KEY_NAME, temporary_query_cache_key).

run_test() -> ?assert(true).

%% まるっきり未登録のlookup動作（キーがなくてundefinedを返す）
store_value_none_test() ->
    query_cache:start_link(),
    Result = query_cache:lookup(<<1,2,3,4>>),
    query_cache:stop(1),
    ?assert(Result == undefined).

%% キーがない場合のlookup動作（キーがなくてundefinedを返す）
store_value0_test() ->
    query_cache:start_link(),
    query_cache:store_key(<<2,3,4,5>>),
    _R1 = query_cache:store_value(<<10,10,10,10>>),
    Result = query_cache:lookup(<<1,2,3,4>>),
    query_cache:stop(1),
    ?assert(Result == undefined).

%% store_valueの戻り値の確認
store_value1_test() ->
    query_cache:start_link(),
    query_cache:store_key(<<1,2,3,4>>),
    Result = query_cache:store_value(<<10,10,10,10>>),
    query_cache:stop(1),
    ?assert(Result == {<<1,2,3,4>>, <<10,10,10,10>>}).

%% 登録が期待通りの場合のlookup動作
store_value2_test() ->
    query_cache:start_link(),
    query_cache:store_key(<<1,2,3,4>>),
    _R1 = query_cache:store_value(<<10,10,10,10>>),
    Result = query_cache:lookup(<<1,2,3,4>>),
    query_cache:stop(1),
    ?assert(Result == <<10,10,10,10>>).

%% キーが上書きされてしまったあとのバリュー保管、という場合のlookup動作（キーがなくてundefinedを返す）
store_value3_test() ->
    query_cache:start_link(),
    query_cache:store_key(<<1,2,3,4>>),
    query_cache:store_key(<<2,3,4,5>>),
    _R1 = query_cache:store_value(<<10,10,10,10>>),
    Result = query_cache:lookup(<<1,2,3,4>>),
    query_cache:stop(1),
    ?assert(Result == undefined).

%% フェーズずれでバリューが投入されたとき（K-V-V）のlookup動作（キーがなくて{undefined, V}になり、保管されない）
store_kv01_test() ->
    query_cache:start_link(),
    query_cache:store_key(<<1,2,3,4>>),
    _R1 = query_cache:store_value(<<10,10,10,10>>),
    R2 = query_cache:store_value(<<22,22,22,22>>),
    _Result = query_cache:lookup(<<1,2,3,4>>),
    query_cache:stop(1),
    ?assert(R2 == {undefined, <<22,22,22,22>>}).
