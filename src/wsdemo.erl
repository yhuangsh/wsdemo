-module(wsdemo).

%% API exports
-export([main/1]).
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-export([client_input_loop/1]).

%% escript Entry point
main(Args) ->
    {ok, Apps} = application:ensure_all_started(gun),
    {ok, Apps1} = application:ensure_all_started(cowboy),
    io:format("Starting dependent apps: ~p~n", [Apps ++ Apps1]),
    case Args of
        ["client", Server, Port] ->
            do_client(Server, list_to_integer(Port));
        ["server"] ->
            do_server();
        _ ->
            io:format("wsdemo client | server~n"),
            erlang:halt(1)
    end.

% Client
do_client(Server, Port) ->
    io:format("Connecting to ~p:~p~n", [Server, Port]),
    {ok, Conn} = gun:open(Server, Port, ws_opts()),
    {ok, Proto} = gun:await_up(Conn, connection_timeout()),
    io:format("Connected! Protocol=~p~n", [Proto]),
    Stream = gun:ws_upgrade(Conn, ws_path()),
    receive
        {gun_upgrade, Conn, Stream, [<<"websocket">>], Headers} ->
            io:format("Websocket upgrade successful! "
                      "Conn=~p~n Stream=~p~n Headers=~p~n", 
                      [Conn, Stream, Headers]),
            PidClientInputLoop = spawn_link(?MODULE, client_input_loop, 
                [self()]),
            client_loop(PidClientInputLoop, Conn, Stream);
        {gun_error, Conn, Stream, Reason} ->
            io:format("Websocket upgrade unsuccessful! "
                      "Conn=~p~n Stream=~p~n Reason=~p~n", 
                      [Conn, Stream, Reason]),
            erlang:halt(2);
        {gun_response, Conn, Stream, IsFin, Status, Headers} ->
            io:format("Websocket upgrade unsucesssful! "
                      "Conn=~p~n Stream=~p~n IsFin=~p~n Status=~p "
                      "Headers=~p~n", 
                      [Conn, Stream, IsFin, Status, Headers]),
            erlang:halt(3)
    after data_timeout() ->
        erlang:halt(4)
    end,
    erlang:halt(0).

client_loop(PidClientInputLoop, Conn, Stream) ->
    io:format("client_loop: ~p~n", [self()]),
    receive
        {send, "end"} ->
            gun:ws_send(Conn, {close, 1000, "Server existing..."}),
            gun:close(Conn),
            io:format("client_loop: bye!~n"),
            exit(PidClientInputLoop, kill),
            erlang:halt(5);
        {send, Message} ->
            gun:ws_send(Conn, {text, Message}),
            client_loop(PidClientInputLoop, Conn, Stream);
        {gun_ws, Conn, Stream, Frame} ->
            io:format("client_loop: received ~p~n", [Frame]),
            client_loop(PidClientInputLoop, Conn, Stream);
        Else ->
            io:format("client_loop: unknown message ~p~n", [Else]),
            exit(PidClientInputLoop, kill),
            erlang:halt(6)
    end.

client_input_loop(PidClientLoop) ->
    io:format("client_input_loop: client_loop pid ~p~n", [PidClientLoop]),
    Input = string:trim(io:get_line("Enter a message: "), trailing, "\n"),
    PidClientLoop ! {send, Input},
    client_input_loop(PidClientLoop).

% Server
do_server() -> 
    PidServerLoop = spawn_link(fun server_loop/0),
    Dispatch = cowboy_router:compile(
        [
            {'_', [{"/", wsdemo, [PidServerLoop]}]}
        ]
    ),
    {ok, _} = cowboy:start_clear(
        websocket_server_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    server_input_loop(PidServerLoop).

server_loop() -> server_loop([]).
server_loop(Conns) -> 
    receive
        {send, Text} ->
            io:format("server_loop: async message to all clients ~p~n",
                [Conns]),
            lists:foreach(fun(Conn) -> Conn ! {async_send, Text} end, Conns),
            server_loop(Conns);
        {addcon, Conn} ->
            io:format("server_loop: add connection ~p~n", [Conn]),
            server_loop([Conn | Conns]);
        {delcon, Conn} ->
            io:format("server_loop: del connection ~p~n", [Conn]),
            server_loop(lists:delete(Conn, Conns))
    end.
    
server_input_loop(PidServerLoop) ->
    io:format("server_input_loop: enter text to send to all clients, enter"
        " \"end\" to stop server. "),
    case string:trim(io:get_line("Enter a message: "), trailing, "\n") of
        "end" ->
            ok;
        Message ->
            PidServerLoop ! {send, Message},
            server_input_loop(PidServerLoop)
    end.

% websocket server callbacks
init(Req, State) ->
    io:format("WS init() State: ~p~n", [State]),
    {cowboy_websocket, Req, State}.

websocket_init([PidServerLoop] = State) ->
    io:format("WS websocket_init() State: ~p~n", [State]),
    io:format("Pid: ~p new client~n", [self()]),
    PidServerLoop ! {addcon, self()},
    {[], State}.

websocket_handle(Frame, State) ->
    io:format("WS websocket_handle() State: ~p~n", [State]),
    io:format("Pid: ~p frame: ~p~n", [self(), Frame]),
    {[Frame], State}.

websocket_info({async_send, Text}, State) ->
    io:format("WS websocket_info() sending async text: ~p~n", [Text]),
    {[{text, Text}], State}.

terminate(Reason, PartialReq, [PidServerLoop] = State) ->
    io:format("WS terminate() State: ~p~n", [State]),
    io:format("Pid: ~p terminating: ~p, ~p~n", [self(), Reason, PartialReq]),
    PidServerLoop ! {delcon, self()},
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

% Default Configurtations

ws_opts() -> #{protocols => [http]}.
ws_path() -> "/".

connection_timeout() -> 15000.

data_timeout() -> 5000.

