-module(tcpclient).
-export([start_server/1, connect/2, recv_loop/3]).

-define(LISTEN_PORT, 9000).
-define(TCP_OPTS, [binary, 
		   {packet, raw}, 
		   {nodelay, true}, 
		   {reuseaddr, true}, 
		   {active, once}]).

%%%%%%%%%%%%%%%%%
%%
%%   Start up the server
%%
%%%%%%%%%%%%%%%%%

start_server(ServerPID) ->
    case gen_tcp:listen(?LISTEN_PORT, ?TCP_OPTS) of
	{ok, Listen} ->
	    spawn(?MODULE, connect, [ServerPID, Listen]),
	    io:format("~p Server started.~n", [erlang:localtime()]);
	Error ->
	    io:format("Error: ~p~n", [Error])
    end.

%%%%%%%%%%%%%%%%%
%%
%%   Connect the accept socket
%%
%%%%%%%%%%%%%%%%%

connect(ServerPID, Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    inet:setopts(Socket, ?TCP_OPTS),
    io:format("Client connected.", []),
    spawn(fun() -> connect(ServerPID, Listen) end),
    recv_loop(ServerPID, Socket, []),
    gen_tcp:close(Socket).

%%%%%%%%%%%%%%%%%
%%
%%   Main event loop
%%
%%%%%%%%%%%%%%%%%

recv_loop(ServerPID, Socket, SoFar) ->
    inet:setopts(Socket, [{active, once}]),
    receive

	% Manage the TCP socket

	{tcp, Socket, Data} ->
	    L1 = SoFar ++ binary_to_list(Data),
	    {Requests, Rest} = split_all(L1, []),
	    lists:map(fun(Req) -> parse_line(ServerPID, Req, Socket) end, 
		      Requests),
	    recv_loop(ServerPID, Socket, Rest);
	{tcp_closed, Socket} ->
	    io:format("~p Client Disconnected.~n", [erlang:localtime()]),
	    ServerPID ! {self(), forced_logout};

	% Manage callbacks from the server

	{error, ReqId, Msg} ->
	    io:format("Error from server: ~p~n", [Msg]),
	    Response=encode_response(ReqId, "error:" ++ Msg),
	    gen_tcp:send(Socket, Response),
	    recv_loop(ServerPID, Socket, SoFar);
	{login, ReqId, BoardX, BoardY, X, Y, Dir, Size, Users} ->
	    io:format("Logged in~n", []),
	    Args=string:join([erlang:integer_to_list(BoardX), 
			      erlang:integer_to_list(BoardY),
			      erlang:integer_to_list(X),
			      erlang:integer_to_list(Y),
			      atom_to_list(Dir),
			      erlang:integer_to_list(Size)] ++
				 Users,
			     ","),
	    Response=encode_response(ReqId, Args),
	    gen_tcp:send(Socket, Response),
	    recv_loop(ServerPID, Socket, SoFar);
	{logout, ReqId} ->
	    io:format("Logged out", []),
	    Response=encode_response(ReqId, "ok"),
	    gen_tcp:send(Socket, Response);
		       
	{moved, ReqId, X, Y, Dir} ->
	    Args=string:join([erlang:integer_to_list(X),
			      erlang:integer_to_list(Y),
			      atom_to_list(Dir)],
			     ","),
	    Response=encode_response(ReqId, Args),
	    gen_tcp:send(Socket, Response),
	    recv_loop(ServerPID, Socket, SoFar);
	{shot_missed, ReqId} ->
	    Response=encode_response(ReqId, "miss"),
	    gen_tcp:send(Socket, Response),
	    recv_loop(ServerPID, Socket, SoFar);
	{shot_kill, ReqId, Killed} ->
	    Args=string:join(Killed, ","),
	    Response=encode_response(ReqId, "kill:" ++ Args),
	    gen_tcp:send(Socket, Response),
	    recv_loop(ServerPID, Socket, SoFar);
	{taunt, ReqId} ->
	    io:format("Taunt successful.~n", []),
	    Response=encode_response(ReqId, "ok"),
	    gen_tcp:send(Socket, Response),
	    recv_loop(ServerPID, Socket, SoFar);
	
	% Manage updates from the server

	{killed, Killer, X, Y} ->
	    Loc=erlang:integer_to_list(X) ++ "," ++
		erlang:integer_to_list(Y),
	    Response=encode_update(["killed", Killer, Loc]),
	    gen_tcp:send(Socket, Response),
	    recv_loop(ServerPID, Socket, SoFar);
	{taunted, FromName, Msg} ->
	    io:format("Taunted by ~p: ~p~n", [FromName, Msg]),
	    Response=encode_update(["taunted", FromName,Msg]),
	    gen_tcp:send(Socket, Response),
	    recv_loop(ServerPID, Socket, SoFar);
	{user_login, Name} ->
	    Response=encode_update(["userlogin", Name]),
	    gen_tcp:send(Socket, Response),
	    recv_loop(ServerPID, Socket, SoFar);
	{user_logout, Name} ->
	    Response=encode_update(["userlogout", Name]),
	    gen_tcp:send(Socket, Response),
	    recv_loop(ServerPID, Socket, SoFar);
	{user_killed, Name} ->
	    Response=encode_update(["userkilled", Name]),
	    gen_tcp:send(Socket, Response),
	    recv_loop(ServerPID, Socket, SoFar);
	
	% debug commands
	{list_users, ReqId, Users} ->
	    Response=encode_response(ReqId, string:join(Users, ",")),
	    gen_tcp:send(Socket, Response),
	    recv_loop(ServerPID, Socket, SoFar)
	  

    end.

%%%%%%%%%%%%%%%%%
%%
%%   Parse a single line and check the requestid.
%%
%%%%%%%%%%%%%%%%%

parse_line(SPid, Line, Socket)->
    io:format("parse_line(~p).~n", [Line]),
    case string:to_integer(Line) of
	{error, no_integer} ->
	    gen_tcp:send(Socket, "error:*:Incorrect requestId provided\r\n");
	{ReqId, Command} ->
	    io:format("parse_line(~p). ReqId: ~p, Command: ~p~n", 
		      [Line, ReqId, Command]),
	    parse_cmd(SPid, ReqId, Command)
    end.

% Handle the command parsing
parse_cmd(SPid, ReqId, ":login:" ++ Username) ->
    io:format("login command: ~p~n", [Username]),
    SPid ! {self(), ReqId, login, Username};
parse_cmd(SPid, ReqId, ":logout") ->
    io:format("logout command: ~n", []),
    SPid ! {self(), ReqId, logout};
parse_cmd(SPid, ReqId, ":move:forward") ->
    SPid ! {self(), ReqId, move, forward};    
parse_cmd(SPid, ReqId, ":move:back") ->
    SPid ! {self(), ReqId, move, back};
parse_cmd(SPid, ReqId, ":move:left") ->
    SPid ! {self(), ReqId, move, left};
parse_cmd(SPid, ReqId, ":move:right") ->
    SPid ! {self(), ReqId, move, right};
parse_cmd(SPid, ReqId, ":shoot:" ++ Coor) ->
    [X, Y] = parse_cs_integer(Coor),
    SPid ! {self(), ReqId, shoot, X, Y};
parse_cmd(SPid, ReqId, ":taunt:" ++ Args) ->
    case string:tokens(Args, ":") of
	[Name, Message] ->
	    SPid ! {self(), ReqId, taunt, Name, Message};
	Other ->
	    self() ! {error, ReqId, "Not valid format: " ++ Other}
    end;
parse_cmd(SPid, ReqId, ":list_users" ) ->
    SPid ! {self(), ReqId, list_users};
parse_cmd(_SPid, _ReqId, Other) ->
    io:format("Unknown command: [~p]~n", [Other]).

%%%%%%%%%%%%%%%%%
%%
%%   Helper functions
%%
%%%%%%%%%%%%%%%%%


%%
%%   Split a line of comma separated integers and convert them
%%
parse_cs_integer(Line) ->
    {Int, Rest} = string:to_integer(Line),
    parse_cs_integer(Rest, [Int]).
parse_cs_integer([], Out) ->
    lists:reverse(Out);
parse_cs_integer("," ++ T, Out) ->
    {Int, Rest} = string:to_integer(T),
    parse_cs_integer(Rest, [Int|Out]).


%%
%%   Split a number of lines into many string, including
%%   a separate split between the \n terminated lines
%%   and the rest of the data if present.
%%
split_all(In, Out) ->
    case split(In, []) of
	more -> {Out, In};
	{Req, Rest} ->
	    split_all(Rest, [Req|Out])
    end.

split("\r\n" ++ T, L) -> {lists:reverse(L), T};    
split("\n" ++ T, L) -> {lists:reverse(L), T};
split([H|T], L) -> split(T, [H|L]);
split([], _) -> more.
			     
encode_response(ReqId, Info) ->
    string:join(["response",
		 erlang:integer_to_list(ReqId),
		 Info], ":") ++ "\r\n".

encode_update(Args) ->
    string:join(["update"] ++ Args, ":") ++ "\r\n".
