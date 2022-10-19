-module(query_cache).

-export([start_link/0]).
-export([stop/1]).

-export([store_key/1]).
-export([store_value/1]).

-export([lookup/1]).

-define(TBLNAME, cache).
-define(PDIC_KEY_NAME, tmp_key).

%%
%% キー（クエリ）が届く：store_keyが呼ばれる
%% バリュー（レスポンス）が届く：store_valueが呼ばれる（対応するクエリはわからない、という想定）
%%　→2つそろったらKVとして保存：
%%
%% キーが届く→最新のキーとして一時保管（古い一時保管キーがあったら上書き
%% バリューが届く
%%  →キーとセットで保管（一時保管キーがなかったら廃棄
%%  一時保管キーは破棄
%%
%% 過去に保管してあったけど新しいキーが来た→キーは上書き、古いのは忘れる（対応するバリューが来なかったんだから仕方ない）
%% 保管したキーで検索→バリューを返す
%% 未保管のキーで検索→undefinedを返す

%% ETS内はすべてlistで保存されている。バイナリは引数でもらったら全部入り口でlist化する

start_link() ->
    ?TBLNAME = ets:new(?TBLNAME, [set, named_table]),
    io:format("query_cache:start_link called~n", []),
    {ok, self()}.

stop(_) ->
    io:format("query_cache:stop called~n", []),
    ets:delete(?TBLNAME).

lookup(BinQuery) when is_binary(BinQuery)->
    ListedQuery = binary_to_list(BinQuery),
    case ets:lookup(?TBLNAME, ListedQuery) of
        [] ->
            undefined;
        [{ListedQuery, ListedValue}] ->
            list_to_binary(ListedValue)
    end.

store_key(BinQuery) when is_binary(BinQuery) ->
    ListedQuery = binary_to_list(BinQuery),
    put(?PDIC_KEY_NAME, ListedQuery).

store_value(BinResponse) when is_binary(BinResponse) ->
    ListedResponse = binary_to_list(BinResponse),
    %% process dictはlistで保持する
    case get(?PDIC_KEY_NAME) of
        undefined ->
            %% nothing to do .
            {undefined, list_to_binary(ListedResponse)};
        ListedLatestKey ->
            %% KV確定
            KvPair = {ListedLatestKey, ListedResponse},
            %% store KV pair into ETS.
            ets:insert(?TBLNAME, KvPair),
            %% erase LatestKey from temporary store
            erase(?PDIC_KEY_NAME),
            %% return result.
            {list_to_binary(ListedLatestKey), list_to_binary(ListedResponse)}
    end.

