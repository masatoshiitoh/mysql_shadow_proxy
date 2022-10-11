-module(socket_client).

-export([start_client/2]).
-export([loop/2]).
-export([send/2]).
-export([proxy_one/3]).
-export([received/2]).

start_client(Address, Port) ->
    Me = self(),
    ClientPid = spawn(?MODULE, proxy_one, [Me, Address, Port]),
    io:format("start_client spawn proxy_one! ClientPid = ~w ~n", [ClientPid]),
    {ok, ClientPid}.

send(Pid, D) ->
    Pid ! {send, D}.

received(Pid, D) ->
    Pid ! {received, D}.

% serve proxy to accepted caller
proxy_one(OriginPid, Address, Port) ->
    io:format("start_client enter proxy_one! ~w ~n", [{OriginPid, Address, Port}]),
    case gen_tcp:connect(Address, Port, [{active, true}, binary], 500) of
        {ok, S} ->
            io:format("start_client connected! ~w ~n", [{ok, S}]),
            loop(S, OriginPid);
        Other ->
            io:format("start_client failed! ~w ~n", [Other]),
            Other
    end.

loop(S, OriginPid) ->
    inet:setopts(S, [{active, once}]),
    receive
        {tcp, _S, Data} ->
            % receive data from server
            received(OriginPid, Data),
            loop(S, OriginPid);
        {send, Data} ->
            gen_tcp:send(S, Data),
            loop(S, OriginPid);
        {stop} ->
            gen_tcp:close(S),
            io:format("socket_client ~w stopped by [~w]~n", [S, OriginPid]),
            ok
    end.
