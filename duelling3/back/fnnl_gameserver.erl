-module(fnnl_gameserver).
-compile(export_all).



start_server() ->
    register(messenger, spawn(fnnl_gameserver, server, [[]])).



% State is [{PID, Name, X, Y, Dir, Size}, ...]

server(State) ->
    receive
	{From, login, Name} ->
	    New_State = server_login(From, Name, State),
	    server(New_State);
	{From, logout} ->
	    New_State = server_logout(From, State),
	    server(New_State);
	%% {From, move, Dir} ->
	%%     New_State = server_move(From, Dir, State),
	%%     server(New_State);
	%% {From, shoot, DX, DY} ->
	%%     New_State = server_shot(From, DX, DY, State),
	%%     server(New_State);
	{From, taunt, Name, Msg} ->
	    server_taunt(From, Name, Msg, State),
	    server(State)
        %% {From, error} ->
	%%     server_error(From),
	%%     server(State)
    end.

server_login(From, Name, State) ->
    case lists:keymember(Name, 2, State) of 
	true ->
	    From ! {messenger, stop, user_exists},
	    State;
	false ->
	    {X, Y, Dir, Size} = generate_new_position(State),
	    From ! {messenger, logged_on, X, Y, Dir, Size},
	    [{From, Name, X, Y, Dir, Size} | State]
    end.

server_logout(From, State) ->
    lists:keydelete(From, 1, State).
    
server_taunt(FromPID, ToName, Msg, State) ->
    case lists:keysearch(FromPID, 1, State) of
	false ->
	    {error, "Not logged in"};
	{value, {_, FromName, _, _, _, _}} ->
	    case lists:keysearch(ToName, 2, State) of
		false ->
		    {error, "Cound not find " ++ ToName};
		{value, {ToPID, _, X, Y, Dir, Size}} ->
		    ToPID ! {taunt, FromName, Msg}
	    end
    end.
generate_new_position(State) ->
    {1, 2, 3, 4}.
