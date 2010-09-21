-module(main).
-export([start/2, main/1]).


main([X, Y]) ->
    io:format("Starting server.~n", []),
    BoardX = list_to_integer(atom_to_list(X)),
    io:format("Board X: ~p~n", [BoardX]),
    BoardYList = atom_to_list(Y),
    io:format("Board Y List: ~p~n", [BoardYList]),
    BoardY = list_to_integer(BoardYList),
    io:format("Board Y: ~p~n", [BoardY]),
    io:format("Starting server.~n", []),
    start(BoardX, BoardY).

start(BoardX, BoardY) ->
    Gameserver=gameserver:start_server(BoardX, BoardY),
    tcpclient:start_server(Gameserver).
