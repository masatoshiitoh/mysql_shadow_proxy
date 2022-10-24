-module(query_cache).
-behaviour(gen_server).

%% API
-export([start/1, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {dummy}).
start(Name) ->
    _sup:start_child(Name).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{dummy=1}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

lookup()

store_key()

store_value()



-export([start_link/0]).
-export([stop/1]).

-export([store_key/1]).
-export([store_value/1]).

-export([lookup/1]).
-export([initialize_kvs/0]).

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
%%
%% ETS内はすべてlistで保存されている。バイナリは引数でもらったら全部入り口でlist化する
%%

start_link() ->
    Pid = spawn(?MODULE, initialize_kvs, []),
    register(cache, Pid),
    {ok, Pid}.

initialize_kvs() ->
    KVS = erase(),
    io:format("query_cache:initialize_kvs called. existing process dict. is ~w~n", [KVS]).

stop(_) ->
    io:format("query_cache:stop called~n", []),
    ok.

lookup(BinQuery) when is_binary(BinQuery)->
    case get(BinQuery) of
        undefined ->
            undefined;
        BinValue ->
            BinValue
    end.

store_key(BinQuery) when is_binary(BinQuery) ->
    put(?PDIC_KEY_NAME, BinQuery).

store_value(BinResponse) when is_binary(BinResponse) ->
    %% process dictはlistで保持する
    case get(?PDIC_KEY_NAME) of
        undefined ->
            %% nothing to do .
            {undefined, BinResponse};
        BinLatestKey ->
            %% store KV pair into ETS.
            put(BinLatestKey, BinResponse),
            %% erase LatestKey from temporary store
            erase(?PDIC_KEY_NAME),
            %% return result.
            {BinLatestKey, BinResponse}
    end.

