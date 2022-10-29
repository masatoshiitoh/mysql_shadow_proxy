-module(listener).

-export([start_link/0]).
-export([stop/1]).
-export([server/1]).

-compile(export_all).

%debug(Format, Data) ->
%    error_logger:info_msg(Format, Data).

stop_children([]) ->
    ok;
stop_children(Pids) ->
    [{pid, Pid} | T] = Pids,
    Pid ! {stop},
    stop_children(T).

stop(_) ->
    ServerPids = ets:lookup(childprocs, pid),
    stop_children(ServerPids).

start_link() ->
    start_link(5, 10001).

start_link(Num, LPort) ->
    ets:new(childprocs, [bag, named_table]),
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
    %%io:format("Loop! Socket [~w]~n", [CallerSocket]),

    receive
        % Caller -> Me -> Server
        {tcp, _CallerSocket, Data} ->
            % request arrived from origin.
            % transfer request bytes to server.
            Req = process_request(Data),
            socket_client:send(ClientPid, Req),
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

process_response(D) ->
    StoreResult = query_cache:store_value(D),
    case StoreResult of
        {undefined, _X} ->
            %%io:format("RESP Payload ~w ~n", [D]),
            D;
        _StoreSucceeded ->
            %%io:format("[STORE]RESP Payload ~w ~n", [StoreSucceeded])
            D
    end.

process_request(D) ->
    {_HeaderBin, BodyBin} = split_binary(D, 4),
    Processed =
        case BodyBin of
            <<>> ->
                D;
            _ELSE ->
                {ComId, PayloadBody} = split_binary(BodyBin, 1),
                case ComId of
                    % COM_INIT_DB
                    <<2>> ->
                        D;
                    % COM_QUERY
                    <<3>> ->
                        case query_classifier:is_modify_operator(PayloadBody) of
                            true ->
                                %%io:format("Header ~w ~n", [HeaderBin]),
                                %%io:format("[STORE]MODIFIER! Payload ~s ~n", [BodyBin]),
                                query_cache:store_key(PayloadBody),
                                D;
                            false ->
                                %%io:format("Header ~w ~n", [HeaderBin]),
                                %%io:format("Payload ~s ~n", [BodyBin]),
                                D
                        end;
                    _OTHER ->
                        D
                end
        end,
    Processed.
