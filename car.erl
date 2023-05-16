-module(car).
-export([main/0, friendship/0, state/0, detect/0]).
-include("utils.hrl").

main() ->
    process_flag(trap_exit, true),
    % io:format("Car: starting~n"),
    % Attore state
    S = spawn(?MODULE, state, []), 
    % Attore detect
    D = spawn(?MODULE, detect, []),
    % Attore friendship
    F = spawn_link(?MODULE, friendship, []),
    monitor(process, F),

    % Registrazione dei pid dei processi
    wellknown ! {register_pid, {F, S}},

    % Invio dei pid agli attori della macchina
    F ! {pid, S, D},
    D ! {pid, S, F},
    S ! {pid, F, D},
    loop(F, S, D).

loop(F, S, D) ->
    receive
        {'EXIT', F, _reason} ->
            loop(F, S, D);

        {'EXIT', _, _reason} ->
            exit(_reason);

        {'DOWN', _Ref, process, _F, _Reason} ->
            demonitor(_Ref, [flush]),

            io:format("Car: friendship component died~n"),
            Snew = spawn(?MODULE, state, []),
            Dnew = spawn(?MODULE, detect, []),
            Fnew = spawn_link(?MODULE, friendship, []),

            wellknown ! {replace_pid, {F, S}, {Fnew, Snew}},
            Fnew ! {pid, Snew, Dnew},
            Dnew ! {pid, Snew, Fnew},
            Snew ! {pid, Fnew, Dnew},

            loop(Fnew, Snew, Dnew);
        _ ->
            io:format("Car: unknown message~n"),
            loop(F, S, D)
    end.   

% FRIENDSHIP COMPONENT
friendship() ->
    receive {pid, S, D} ->
        link(S),
        link(D),
        friendship([], S, D)
    end.


% Friends -> List of friends
% MyState -> State process of caller car
% MyDetect -> Detect process of caller car
friendship(Friends, MyState, MyDetect) ->
    case length(Friends) < ?MAX_FRIENDS of
        true ->
            AllMyFriends = ask_for_friends(MyState, Friends),            
            [ monitor(process, F) || {F, _} <-lists:subtract(AllMyFriends, Friends)],
            render ! {friendship, MyState, AllMyFriends},
            friendship(AllMyFriends, MyState, MyDetect);
        false ->
            receive
                {getFriends, PID1, PID2, Ref} -> 
                    NewFriends = lists:delete({PID1,PID2}, Friends),
                    PID1 ! {myFriends, NewFriends, Ref},
                    friendship(Friends, MyState, MyDetect);
                {getOwnFriends, MyState, Ref} ->
                    MyState ! {myFriends, Friends, Ref};
                {'DOWN', _Ref, process, _DeadPid, _Reason} ->
                    demonitor(_Ref, [flush]),
                    friendship(remove_dead_friend(Friends, _DeadPid), MyState, MyDetect)
            end
    end.


% S -> State process of caller car
% FriendsLisr -> Actual list of friends of caller car
ask_for_friends(S,FriendsList) ->
    ask_for_friends(S, FriendsList, FriendsList).

% S -> State process of caller car
% Friends -> Friends of caller car
% NewFriendsList -> List of new friends
ask_for_friends(S, [{F,_} | T], FriendsList) ->
    F ! {getFriends, self(), S, Ref = make_ref()},
    wait_for_response(S, T, FriendsList, Ref);

% If the list is empty, search for new friends through wellknown actor
ask_for_friends(S, [], FriendsList) ->
    wellknown ! {getFriends, self(), S, Ref = make_ref()},
    wait_for_response(S, FriendsList, FriendsList, Ref).


% S -> State process of caller car
% F -> List of friends of caller car
% FriendsList -> List of friends of caller car
% Ref -> Reference of the message
wait_for_response(S, F, FriendsList, Ref) ->
    receive
        {getFriends, PID1, PID2, NewRef} -> 
            NewFriends = lists:delete({PID1,PID2}, FriendsList),
            PID1 ! {myFriends, NewFriends, NewRef},
            MyNewFriends = lists:usort(FriendsList ++ NewFriends),
            case length(MyNewFriends) < ?MAX_FRIENDS of
                true ->
                    ask_for_friends(S, F, MyNewFriends);
                false ->
                    MyNewFriends
            end;
        {myFriends, NewFriends, Ref} ->
            NewFriendsList = lists:usort(FriendsList ++ NewFriends),
            case length(NewFriendsList) < ?MAX_FRIENDS of
                true ->
                    ask_for_friends(S, F, NewFriendsList);
                false ->
                    NewFriendsList
            end;
        {'DOWN', _Ref, process, _Pid, _Reason} ->
            demonitor(Ref, [flush]),
            wait_for_response(S, remove_dead_friend(F, _Pid), remove_dead_friend(FriendsList, _Pid), Ref)
    end.


% remove_dead_friend: (list, pid) -> list
% rimuove dalla lista di amici 
% la coppia con il pid del processo morto
remove_dead_friend(Friends, DeadFriend) -> lists:filter(fun({F, _}) -> F =/= DeadFriend end, Friends).



% STATE COMPONENT
state() ->
    Grid = utils:init_grid(sconosciuto),
    receive {pid, F, D} ->
        link(F),
        link(D),
        state(F, D, Grid, 0,0)
    end.

% Take the information of the Grid's cells.
% When a new information is discoverd, send a message to the friends (state components)
% If the goal is occupied, change it
% F -> pid of friendship component
% D -> pid of detect component
% Grid -> grid of the ambient
% TX, TY -> target position
state(F, D, Grid, TX, TY) ->
    receive
        {askTarget, D, _, _, Ref} ->
            {NX, NY} = utils:find_free_cell(Grid),
            D ! {newTarget, NX, NY, Ref},
            state(F, D, Grid, NX, NY),
            render ! {status, self(), "Sto dando un nuovo target"};
        % receive the state from Detect to update the Grid and notify the new state
        % if the newState is different from the old one, send a message to the friends
            % send the message to friends
        {notifyStatus, X, Y, IsFree} -> 
            NewState = case IsFree of 
                true -> libero;
                false -> occupato
            end,

            render ! {status, self(), "Ho ricevuto una notifica, cambio stato"},
            OldState = utils:get_state(Grid, X, Y),      
            utils:set_state(Grid, X, Y, NewState),         
            case OldState =/= IsFree of
                true -> notify_friends(F, IsFree, X, Y)
            end,

            case ({TX, TY} =:= {X, Y}) and NewState =:= occupato of
                true ->
                    {NX, NY} = utils:find_free_cell(Grid),
                    D ! {updateTarget, self(), {NX, NY}},
                    render ! {status, self(), "Sto dando un nuovo target dopo gossiping"},
                    state(F, D, Grid, NX, NY);
                false ->
                    state(F, D, Grid, TX, TY)
            end
    end.

notify_friends(F, State, X, Y) ->
    F ! {getOwnFriends, self()},
    receive
        {myFriends, Friends} -> [S ! {notifyStatus, {X, Y}, State} || {_, S} <- Friends]
    end.


% DETECT COMPONENT
detect() ->
    receive {pid, S, F} ->
        link(S),
        link(F),
        POS = {rand:uniform(?GRID_HEIGHT), rand:uniform(?GRID_WIDTH)},
        detect(S, POS)
    end.

% If no target is set, ask for a new one to State
detect(S, {XP,YP}) -> 
    S ! {askTarget, self(), XP, YP, Ref = make_ref()},
    receive
        {newTarget, XT, YT, Ref} ->
            render ! {target, S, XT, YT},
            render ! {position, S, XP, YP},
            render ! {parked, S, XP, YP, false},
            detect(S, {XP,YP}, {XT,YT})
    end.

detect(S, {XP,YP}, {XT,YT} ) ->
    timer:sleep(?TIME_STEP),
    {NEW_XP, NEW_YP} = move(XP, YP, XT, YT), 
    render ! {status, "Sono in movimento"},
    ambient ! {isFree, self(), NEW_XP, NEW_YP, Ref = make_ref()},
    render ! {position, S, NEW_XP, NEW_YP},
    detect_response(S, {NEW_XP, NEW_YP}, {XT,YT}, Ref).

detect_response(S, {XP,YP}, {XT,YT}, Ref) ->
    receive 
        {updateTarget, S, {NEW_XT, NEW_YT}} ->
            render ! {target, S, XT, YT},
            detect_response(S, {XP, YP}, {NEW_XT, NEW_YT}, Ref);
        {status, Ref, IsFree} ->
            S ! {notifyStatus, XP, YP, IsFree},  
            case {XP, YP} =:= {XT, YT} andalso IsFree of
                true ->
                    NewRef = make_ref(),
                    % Car is parking 
                    ambient ! {park, self(), XT, YT, NewRef},
                    render ! {parked, S, XP, YP, true},
                    render ! {status, "Sono parcheggiato"},
                    % Car is parked and waiting 
                    Time = rand:uniform(5),
                    timer:sleep(Time * 1000),
                    % Car is leaving
                    ambient ! {leave, self(), NewRef},
                    render ! {parked, S, XP, YP, false},
                    render ! {status, "Sono ripartito"},
                    detect(S, {XP,YP});
                false ->
                    detect(S, {XP,YP}, {XT, YT})
            end
    end.

move(Xi, Yi, Xi, Yi) ->
    {Xi, Yi};

% Case in which (Xi, Yi) and (Xf, Yf) are aligned alongside y-axis
move(Xi, Yi, Xi, Yf) ->
    Direction = 2,
    move_along(Xi, Yi, Xi, Yf, Direction);

% Case in which (Xi, Yi) and (Xf, Yf) are aligned alongside x-axis
move(Xi, Yi, Xf, Yi) ->
    Direction = 1,
    move_along(Xi, Yi, Xf, Yi, Direction);

% (Xi, Yi) -> current position
% (Xf, Yf) -> target position 
move(Xi, Yi, Xf, Yf) ->
    Direction = rand:uniform(2),
    move_along(Xi, Yi, Xf, Yf, Direction).

% Direction = 1 -> move alongside x
% Direction = 2 -> move alongside y
move_along(Xi, Yi, Xf, _, 1) ->
    % Compute min between (Xi, Xf) and ((Xi, W) + (W, Xf))

    % Modular distance
    D_m = ?GRID_WIDTH - Xi + Xf,

    % Grid distance
    D_g = abs_dif(Xf,Xi),
    D_g = abs(Xf - Xi),
    
    case min(D_m, D_g) of
        D_g ->
            % signed_norm returns +-1, gives the direction alongside x. 
            Step = signed_norm(Xf, Xi),
            % io:format("Car along x: move ~p, ~p -> ~p, ~p~n", [Xi, Yi, Xi + Step, Yi]),
            {Xi + Step, Yi};
        D_m ->
            Step = signed_norm(?GRID_WIDTH, Xi),

            % Since erlang counts grids from 1, we compute the modular sum r.t. `GRID_WIDTH`.
            % If the modular sum is equal to 0, we want the first position in the grid,
            % so we always compute the max between the modular sum and 1.
            New_X = max(Xi+Step rem (?GRID_WIDTH + 1), 1),
            % io:format("Car along x: move ~p, ~p -> ~p, ~p~n", [Xi, Yi, New_X, Yi]),
            {New_X, Yi}
    end;

move_along(Xi, Yi, _, Yf, 2) ->
    % Compute min between (Yi, Yf) and ((Yi, H) + (H, Yf))

    % Modular distance
    D_m = ?GRID_HEIGHT - Yi + Yf,


    % Grid distance
    % D_g = abs_dif(Yf, Yi),
    D_g = abs(Yf - Yi),
   
    case min(D_m, D_g) of
        D_g ->
            Step = signed_norm(Yf, Yi),
            % io:format("Car along y: move ~p, ~p -> ~p, ~p~n", [Xi, Yi, Xi, Yi + Step]),
            {Xi, Yi + Step};
        D_m ->
            Step = signed_norm(?GRID_HEIGHT, Yi),

            % Since erlang counts grids from 1, we compute the modular sum r.t. `GRID_HEIGHT`.
            % If the modular sum is equal to 0, we want the first position in the grid,
            % so we always compute the max between the modular sum and 1.
            New_Y = max((Yi + Step) rem (?GRID_HEIGHT + 1), 1),
            % io:format("Car along y: move ~p, ~p -> ~p, ~p~n", [Xi, Yi, Xi, New_Y]),
            {Xi, New_Y}
    end.    

signed_norm(Xm, X0) ->
    case Xm - X0 of
        0 -> 0;
        _ -> trunc((Xm - X0) / abs(Xm - X0))
    end.

abs_dif(A,B) ->
    case A - B of
        0 -> 0;
        _ -> abs(A - B)
    end.
