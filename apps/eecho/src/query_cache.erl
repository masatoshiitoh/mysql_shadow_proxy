-module(query_cache).
-behaviour(gen_server).

%% API
%% ここの記載を参考に。
%% https://stacktrace.hatenablog.jp/entry/2019/11/05/190000
%% http://erlang.shibu.jp/design_principles/gen_server.html

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([terminate/2]).

-export([stop/1]).
-export([append/2]).

-export([initialize_kvs/0]).
-export([lookup/1]).
-export([store_key/1]).
-export([store_value/1]).

-define(TBLNAME, cache).
-define(PDIC_KEY_NAME, tmp_key).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, dict:new()}.

terminate(Reason, State) -> ok.

handle_call({store_key, BinQuery}, _From, Dict) ->
    Result = put(?PDIC_KEY_NAME, BinQuery),
    {reply, Result, Dict};
handle_call({store_value, BinResponse}, _From, Dict) ->
    case get(?PDIC_KEY_NAME) of
        undefined ->
            %% nothing to do .
            {reply, {undefined, BinResponse}, Dict};
        BinLatestKey ->
            %% store KV pair into Dict.
            NewDict = dict:append(BinLatestKey, BinResponse, Dict),
            %% erase LatestKey from temporary store
            erase(?PDIC_KEY_NAME),
            %% return result.
            io:format("query_cache: stored pair ~w~n", [{BinLatestKey, BinResponse}]),
            {reply, {BinLatestKey, BinResponse}, NewDict}
    end;
handle_call({find, Key}, _From, Dict) ->
    case dict:find(Key, Dict) of
        error ->
            io:format("query_cache: NOT FOUND: find request ~w~n", [{find, Key}]),
            {reply, undefined, Dict};
        {ok, Value} ->
            io:format("query_cache: FOUND: find request ~w~n", [{find, Key}]),
            %% 見つけたキーは消す
            NewDict = dict:erase(Key, Dict),
            [H | _T] = Value,
            {reply, H, NewDict}
    end.

handle_cast({append, Key, Value}, Dict) ->
    NewDict = dict:append(Key, Value, Dict),
    {noreply, NewDict};
handle_cast(initialize_kvs, _Dict) ->
    erase(?PDIC_KEY_NAME),
    NewDict = dict:new(),
    {noreply, NewDict}.

append(Key, Value) -> gen_server:cast(?MODULE, {append, Key, Value}).

store_key(BinQuery) -> gen_server:call(?MODULE, {store_key, BinQuery}).

store_value(BinResponse) -> gen_server:call(?MODULE, {store_value, BinResponse}).

lookup(Key) -> gen_server:call(?MODULE, {find, Key}).

initialize_kvs() -> gen_server:cast(?MODULE, initialize_kvs).

stop(_X) -> gen_server:stop(?MODULE).

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
%%
%% ETS内はすべてlistで保存されている。バイナリは引数でもらったら全部入り口でlist化する
%%
