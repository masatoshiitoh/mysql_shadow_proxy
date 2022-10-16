-module(query_cache).

-export([start_link/0]).
-export([stop/1]).

-export([store_key/1]).
-export([store_value/1]).

-export([lookup/1]).

-define(TBLNAME, cache).
-define(PDIC_KEY_NAME, temporary_query_cache_key).

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
%% 保管したキーで検索→バリューを返す
%% 未保管のキーで検索→undefinedを返す


start_link() ->
    ?TBLNAME = ets:new(?TBLNAME, [set, named_table]),
    {ok, self()}.

stop(_) ->
    ets:delete(?TBLNAME).

lookup(Query) ->
    case ets:lookup(?TBLNAME, Query) of
        [] ->
            undefined;
        [{Query, Value}] ->
            Value
    end.

store_key(Query) ->
    put(?PDIC_KEY_NAME, Query).

store_value(Response) ->
    case get(?PDIC_KEY_NAME) of
        undefined ->
            %% nothing to do .
            {undefined, Response};
        LatestKey ->
            %% KV確定
            KvPair = {LatestKey, Response},
            %% store KV pair into ETS.
            ets:insert(?TBLNAME, KvPair),
            %% erase LatestKey from temporary store
            erase(?PDIC_KEY_NAME),
            %% return result.
            KvPair
    end.

