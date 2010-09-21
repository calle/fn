-module(gameserver).
-export([start_server/2, server/3]).

start_server(BoardX, BoardY) ->
    spawn(gameserver, server, [BoardX, BoardY, []]).

% State is [{PID, Name, X, Y, Dir, Size}, ...]


%%%%%%%%%%%%%%%%%
%%
%%   Main loop
%%
%%%%%%%%%%%%%%%%%

server(BoardX, BoardY, State) ->
    receive
	{From, ReqId, login, Name} ->
	    New_State = server_login(From, ReqId, Name, BoardX, BoardY, State),
	    server(BoardX, BoardY, New_State);
	{From, ReqId, logout} ->
	    New_State = server_logout(From, ReqId, State),
	    server(BoardX, BoardY, New_State);
	{From, forced_logout} ->
	    New_State = server_logout(From, State),
	    server(BoardX, BoardY, New_State);
	{From, ReqId, move, Movement} ->
	    New_State = move(From, ReqId, Movement, BoardX, BoardY, State),
	    server(BoardX, BoardY, New_State);
	{From, ReqId, shoot, X, Y} ->
	    New_State = shoot(From, ReqId, X, Y, BoardX, BoardY, State),
	    server(BoardX, BoardY, New_State);
	{From, ReqId, taunt, Name, Msg} ->
	    server_taunt(From, ReqId, Name, Msg, State),
	    server(BoardX, BoardY, State);
	{From, ReqId, list_users} ->
	    Users = get_current_users(State),
	    From ! {list_users, ReqId, Users},
	    server(BoardX, BoardY, State)
    end.


%%%%%%%%%%%%%%%%%
%%
%%   Shooting
%%
%%%%%%%%%%%%%%%%%

shoot(From, ReqId, X, Y, BoardX, BoardY, State) when 
      X < 0; X > BoardX;
      Y < 0; Y > BoardY  ->
    From ! {shot_missed, ReqId},
    State;
shoot(From, ReqId, PX, PY, _BoardX, _BoardY, State) ->
    Candidates=lists:filter(fun({_PID, _Name, X, Y, Dir, Size}) ->
				    test_shoot(X, Y, Dir, Size, PX, PY) end,
			    State),
    Hits=lists:keydelete(From, 1, Candidates),
    handle_kills(From, ReqId, Hits, PX, PY, State).


handle_kills(From, ReqId, [], _PX, _PY, State) ->
    From ! {shot_missed, ReqId},
    State;
handle_kills(From, ReqId, Hits, PX, PY, State) ->
    Shooter = get_username(From, State),
    notify_all({killed, Shooter, PX, PY}, Hits),
    Killed = lists:map(fun({_PID, Name, _X, _Y, _Dir, _Size}) ->
			       Name end,
		       Hits),
    From ! {shot_kill, ReqId, Killed},
    handle_dead(State, Hits).
    

handle_dead(State, []) ->
    State;
handle_dead(State, [{PID, _Name, _X, _Y, _Dir, _Size} | Hits]) ->
    case get_username(PID, State) of
	false ->
	    handle_dead(State, Hits);
	Name ->
	    NewState = lists:keydelete(PID, 1, State),
	    notify_all({user_killed, Name}, NewState),
	    handle_dead(NewState, Hits)
    end.


test_shoot(X, Y, Dir, Size, PX, PY ) ->    
    if 
	Dir == north;Dir == south ->
	    test_size(Y-PY, Size, X - PX);
	Dir == east; Dir == west ->
	    test_size(X-PX, Size, Y - PY)
    end.

test_size(Lengthwise, Size, Sidewise ) when
      Lengthwise < - Size/2;
      Lengthwise > Size/2;
      Sidewise /= 0 ->
    false;
test_size(_Lengthwise, _Size, _Sidewise) ->
    true.




%%%%%%%%%%%%%%%%%
%%
%%   Moving
%%
%%%%%%%%%%%%%%%%%

move(From, ReqId, Movement, BoardX, BoardY, State) ->
    case lists:keysearch(From, 1, State) of
	false ->
	    From ! {error, ReqId, "User not logged in"},
	    State;
	{value, {_FromPID, Name, OldX, OldY, Direction, Size}} ->
	    DX = calc_x_movement(Direction, Movement),
	    DY = calc_y_movement(Direction, Movement),
	    NewX = (OldX + BoardX + DX) rem BoardX,
	    NewY = (OldY + BoardY + DY) rem BoardY,
	    NewDir = calc_dir(Direction, Movement),
	    New_Data={From, Name, NewX, NewY, NewDir, Size},
	    New_State=lists:keyreplace(From, 1, State, New_Data),
	    From ! {moved, ReqId, NewX, NewY, NewDir},
	    New_State
    end.
    
% ni får säga vad ni vill men jag orkade inte göra det här snyggare.. :)
calc_x_movement(east, forward) -> 1;
calc_x_movement(east, back)    -> -1;
calc_x_movement(west, forward) -> -1;
calc_x_movement(west, back)    -> 1;
calc_x_movement(north, left)   -> -1;
calc_x_movement(north, right)  -> 1;
calc_x_movement(south, left)   -> 1;
calc_x_movement(south, right)  -> -1;
calc_x_movement(_Dir, _Mov)    -> 0.

calc_y_movement(north, forward) -> 1;
calc_y_movement(north, back)    -> -1;
calc_y_movement(south, forward) -> -1;
calc_y_movement(south, back)    -> 1;
calc_y_movement(east, left)     -> 1;
calc_y_movement(east, right)    -> -1;
calc_y_movement(west, left)     -> -1;
calc_y_movement(west, right)    -> 1;
calc_y_movement(_Dir, _Mov)     -> 0.

calc_dir(north, left) -> west;
calc_dir(north, right) -> east;
calc_dir(south, left) -> east;
calc_dir(south, right) -> west;
calc_dir(east, left) -> north;
calc_dir(east, right) -> south;
calc_dir(west, left) -> south;
calc_dir(west, right) -> north;
calc_dir(Dir, _Mov) -> Dir.


%%%%%%%%%%%%%%%%%
%%
%%   Login
%%
%%%%%%%%%%%%%%%%%

server_login(From, ReqId, Name, BoardX, BoardY, State) ->
    case lists:keymember(Name, 2, State) of 
	true ->
	    From ! {error, ReqId, "User already logged in"},
	    State;
	false ->
	    {X, Y, Dir, Size} = generate_new_position(BoardX, BoardY, State),
	    Users = get_current_users(State),
	    From ! {login, ReqId, BoardX, BoardY, X, Y, Dir, Size, Users},
	    notify_all({user_login, Name}, State),
	    [{From, Name, X, Y, Dir, Size} | State]
    end.

%%%%%%%%%%%%%%%%%
%%
%%   Logout
%%
%%%%%%%%%%%%%%%%%

server_logout(FromPID, ReqId, State) ->
    case get_username(FromPID, State) of
	false ->
	    FromPID ! {error, ReqId, "Not logged in"},
	    State;
	FromName ->
	    NewState = lists:keydelete(FromPID, 1, State),
	    notify_all({user_logout, FromName}, NewState),
	    FromPID ! {logout, ReqId},
	    NewState
    end.

%%%%%%%%%%%%%%%%%
%%
%%   Forced logout
%%
%%%%%%%%%%%%%%%%%

server_logout(FromPID, State) ->
    case get_username(FromPID, State) of
	false ->
	    State;
	FromName ->
	    NewState = lists:keydelete(FromPID, 1, State),
	    notify_all({user_logout, FromName}, NewState),
	    NewState
    end.
    
%%%%%%%%%%%%%%%%%
%%
%%   Taunting
%%
%%%%%%%%%%%%%%%%%

server_taunt(FromPID, ReqId, ToName, Msg, State) ->
    case get_username(FromPID, State) of
	false ->
	    {error, "Not logged in"};
	FromName ->
	    case lists:keysearch(ToName, 2, State) of
		false ->
		    {error, "Cound not find " ++ ToName};
		{value, {ToPID, _, _X, _Y, _Dir, _Size}} ->
		    FromPID ! {taunt, ReqId},
		    ToPID ! {taunted, FromName, Msg}
	    end
    end.


%%%%%%%%%%%%%%%%%
%%
%%  Helper functions
%%
%%%%%%%%%%%%%%%%%

get_username(FromPID, State) ->
    case lists:keysearch(FromPID, 1, State) of
	false ->
	    false;
	{value, {_, FromName, _, _, _, _}} ->
	    FromName
    end.

%% This function currently ignores the State, but
%% can use it to generate better starting coordinates 
%% if need be.
generate_new_position(BoardX, BoardY, _State) ->
    Dir=case random:uniform(4) of
	    0->north;
	    1->east;
	    2->south;
	    3->west;
	    _->north
	end,
    {random:uniform(BoardX), 
     random:uniform(BoardY), Dir, 3}.
%%
%% Get a list of all logged in users.
%%
get_current_users([]) ->
    [];
get_current_users([ {_, Name, _, _, _, _ } | State]) ->
    [Name | get_current_users(State)].

%%
%% Send a message to all currently logged in users.
%%
notify_all(_Message, []) ->
    [];
notify_all(Message, [Single | State])->
    notify(Single, Message),
    notify_all(Message, State).

notify({From, _, _, _, _, _}, Message) ->
    From ! Message.

