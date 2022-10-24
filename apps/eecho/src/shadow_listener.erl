-module(shadow_listener).

-export([start_link/0]).
-export([stop/1]).
-export([server/1]).

%%
%% shadow_proxy モジュール。
%% 方針
%% socketをlistenする。
%% 本物のサーバーに自前ソケットを開くが、
%% 本物のサーバーには直接更新処理を投げない（INSERT/UPDATE/DELETEは投げない）
%% 手順
%% まずは、ただのプロキシで作って、（←今ここ）
%% 本流プロキシの結果をもらうようにして
%% 仕上げていく
%%
stop(_) ->
    ok.

start_link() ->
    start_link(5, 10002).

start_link(Num, LPort) ->
    %%ets:new(childprocs, [bag, named_table]),
    case
        gen_tcp:listen(LPort, [
            binary,
            {reuseaddr, true},
            {active, true},
            {packet, raw}
        ])
    of
        {ok, ListenSock} ->
            ok = start_servers(Num, ListenSock),
            {ok, _Port} = inet:port(ListenSock),
            {ok, self()};
        {error, Reason} ->
            {error, Reason}
    end.

start_servers(0, _) ->
    ok;
start_servers(Num, LS) ->
    ServerPid = spawn(?MODULE, server, [LS]),

    ets:insert(childprocs, {pid, ServerPid}),
    ServerPids = ets:lookup(childprocs, pid),
    io:format("Pids: [~w]~n", [ServerPids]),

    start_servers(Num - 1, LS).

server(LS) ->
    case gen_tcp:accept(LS) of
        {ok, CallerSocket} ->
            io:format("accepted! ~w ~n", [CallerSocket]),
            %
            % new connection arrived.
            % Make new server connection
            X = socket_client:start_client("localhost", 3306),
            io:format("client spawned! ~w ~n", [X]),
            {ok, ClientPid} = X,

            %
            loop(CallerSocket, ClientPid),
            %
            % accepted socket closed.
            % Close server connection.
            %
            server(LS);
        Other ->
            io:format("accept returned ~w - goodbye!~n", [Other]),
            ok
    end.

loop(CallerSocket, ClientPid) ->
    inet:setopts(CallerSocket, [{active, once}]),
    io:format("Loop! Socket [~w]~n", [CallerSocket]),

    receive
        % Caller -> Me -> Server
        {tcp, _CallerSocket, Data} ->
            % request arrived from origin.
            % transfer request bytes to server.
            RequestX = process_request(Data),
            case RequestX of
                {passthru, Request} ->
                    io:format("PassThru [~w]~n", [Request]),
                    socket_client:send(ClientPid, Request);
                {replynow, undefined} ->
                    io:format("ReplyNow not found~n", []),
                    self() ! {tcp, _CallerSocket, Data};
                {replynow, Reply} ->
                    io:format("ReplyNow [~w]~n", [Reply]),
                    gen_tcp:send(CallerSocket, Reply)
            end,

            loop(CallerSocket, ClientPid);
        % Caller <- Me <- Server
        {received, Data} ->
            % response arrived from server
            % transfer response bytes to origin.
            Answer = process_response(Data),
            gen_tcp:send(CallerSocket, Answer),
            loop(CallerSocket, ClientPid);
        % Caller closed socket
        {tcp_closed, CallerSocket} ->
            io:format("Socket ~w closed [~w]~n", [CallerSocket, self()]),
            ClientPid ! {stop},
            ok;
        % This proxy has stopped.
        {stop} ->
            io:format("Socket ~w stopped [~w]~n", [CallerSocket, self()]),
            ClientPid ! {stop},
            gen_tcp:close(CallerSocket),
            ok
    end.

process_request(D) ->
    {HeaderBin, BodyBin} = split_binary(D, 4),
    Processed =
        case BodyBin of
            <<>> ->
                D;
            _ELSE ->
                {ComId, PayloadBody} = split_binary(BodyBin, 1),
                case ComId of
                    % COM_INIT_DB
                    <<2>> ->
                        io:format("REQ Header ~w ~n", [HeaderBin]),
                        io:format("REQ COM_INIT_DB ~n", []),
                        io:format("REQ Payload ~s ~n", [PayloadBody]),
                        {passthru, D};
                    % COM_QUERY
                    <<3>> ->
                        %% modifierだったら即返答（Next Action: キャッシュを待つ）
                        case query_classifier:is_modify_operator(PayloadBody) of
                            true ->
                                CachedResponse = query_cache:lookup(PayloadBody),
                                io:format("MODIFIER_FOUND ~w ~n", [CachedResponse]),
                                {replynow, CachedResponse};
                            _FALSE ->
                                io:format("REQ Header ~w ~n", [HeaderBin]),
                                io:format("REQ COM_QUERY ~n", []),
                                io:format("REQ Payload ~s ~n", [PayloadBody]),
                                io:format("REQ Payload ~w ~n", [PayloadBody]),
                                {passthru, D}
                        end;
                    %OTHER
                    _Other ->
                        io:format("REQ Full ~w ~n", [D]),
                        io:format("REQ ComId Other ~w ~n", [_Other]),
                        {passthru, D}
                end
        end,
    query_cache:store_key(BodyBin),
    Processed.

process_response(D) ->
    io:format("RESP Payload ~w ~n", [D]),
    D.

%%
%% あーーー　レスポンスを返す場所、通常とシャドウで違うんだ。
%% 通常は、サーバーからの返却イベントでCallerSocketに書き込むが、
%% シャドウは、モディファイアのときだけサーバーに投げず
%% （なので、応答パケットも来ないのでprocess_responseがキックされない）
%% キャッシュに入るのを待って、それをCallerSocketに書き込むんだ。
%%
%% だから、process_requestがもっとややこしくなるんだ。
%% (通常側はprocess_requestとprocess_responseが両方ややこしくなった)
%%
%% 作る前にイメージ固めておけよ感があるけど、まあ、間に合ったってことで。
%%
%% そして、プリペアードステートメントでうまくいかないパターンは、ProxySQLと同様、こいつでも
%% 起きそうだな...
%%
%%
%% あ。プロセス辞書が、spawnして参照できてないのでは？？？　あああ。。。
%%
