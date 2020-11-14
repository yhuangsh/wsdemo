-module(wsdemo).

%% API exports
-export([main/1]).
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, Apps} = application:ensure_all_started(gun),
    {ok, Apps1} = application:ensure_all_started(cowboy),
    io:format("Starting dependent apps: ~p~n", [Apps ++ Apps1]),
    case Args of
        ["client", Server] ->
            do_client(Server, 80);
        ["client", Server, Port] ->
            do_client(Server, list_to_integer(Port));
        ["server"] ->
            do_server();
        _ ->
            io:format("wsdemo client | server~n"),
            erlang:halt(1)
    end.

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
            do_echo(Conn, Stream);
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

do_echo(Conn, Stream) ->
    Input = string:trim(io:get_line("Enter a message: "), trailing, "\n"),
    case Input of
        "end" ->
            gun:ws_send(Conn, {close, 1000, "Server existing..."}),
            gun:close(Conn),
            io:format("Bye!~n"),
            erlang:halt(5);
        Input ->
            gun:ws_send(Conn, {text, Input}),
            receive
                {gun_ws, Conn, Stream, Frame} ->
                    io:format("Received: ~p~n", [Frame]);
                Else ->
                    io:format("!Else: ~p~n", [Else]),
                    erlang:halt(6)
            end,
            do_echo(Conn, Stream)
    end.

do_server() -> 
    Dispatch = cowboy_router:compile(
        [
            {'_', [{"/", wsdemo, []}]}
        ]
    ),
    {ok, _} = cowboy:start_clear(
        websocket_server_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    io:get_line("Enter anything to stop server and quit: ").

% websocket callbacks
init(Req, Status) ->
    {cowboy_websocket, Req, Status}.

websocket_init(State) ->
    io:format("~nPid: ~p new client~n", [self()]),
    {[], State}.

websocket_handle(Frame, State) ->
    io:format("~nPid: ~p frame: ~p~n", [self(), Frame]),
    {[Frame], State}.

websocket_info(Info, State) ->
    io:format("~nPid: ~p info: ~p~n", [self(), Info]),
    {[], State}.

terminate(Reason, PartialReq, _State) ->
    io:format(
        "~nPid: ~p terminating: ~p, ~p~n", 
        [self(), Reason, PartialReq]
    ),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

% Default Configurtations

ws_opts() -> #{protocols => [http]}.
ws_path() -> "/".

connection_timeout() -> 15000.

data_timeout() -> 5000.

