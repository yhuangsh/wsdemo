-module(wsdemo).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, Apps} = application:ensure_all_started(gun),
    io:format("Started apps=~p~n", [Apps]),
    io:format("Connecting to ~p, Args: ~p~n", [server(), Args]),
    {ok, Conn} = gun:open(server(), port(), ws_opts()),
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
            erlang:halt(1)
    after data_timeout() ->
        erlang:halt(3)
    end,
    erlang:halt(0).

do_echo(Conn, Stream) ->
    Input = string:trim(io:get_line("Enter a message: "), trailing, "\n"),
    case Input of
        "end" ->
            gun:close(Conn),
            io:format("Bye!~n"),
            erlang:halt(0);
        Input ->
            gun:ws_send(Conn, {text, Input}),
            receive
                {gun_ws, Conn, Stream, Frame} ->
                    io:format("Received: ~p~n", [Frame]);
                Else ->
                    io:format("!Else: ~p~n", [Else]),
                    erlang:halt(4)
            end,
            do_echo(Conn, Stream)
    end.
%%====================================================================
%% Internal functions
%%====================================================================

% Default Configurtations

server() -> "echo.wss-websocket.net".

port() -> 443.

ws_opts() -> #{protocols => [http]}.
ws_path() -> "/".

connection_timeout() -> 15000.

data_timeout() -> 5000.

