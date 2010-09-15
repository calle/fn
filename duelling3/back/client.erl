-module(client).
-compile(export_all).

client_proc() ->
    receive
	{taunt, FromName, Msg} ->
	    io:format("You were taunted by ~p: ~p~n", [FromName, Msg]);
	{PID, logged_on, X, Y, Dir, Size} ->
	    io:format("Yes", []);
	{messenger, stop, user_exists} ->
	    io:format("No", [])
    end,
    client_proc().



start_client(Name) ->
    spawn(client, client_proc, []).
%    login(mes_client, Name).

login(PID, Name)->
    messenger ! { PID, login, Name}.


logout()->	    
    messenger ! { self(), logout}.

taunt(PID, Name, Msg) ->
    messenger ! { PID, taunt, Name, Msg}.
