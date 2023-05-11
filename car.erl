-module(car).
-export([main/0, friendship/0, state/0, detect/0]).
-define(MAX_FRIENDS,5).

main() ->
    process_flag(trap_exit, true),
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
        {'EXIT', _, _reason} ->
            exit(_reason);

        % DOWN: il processo è morto
        % process: tipo di messaggio
        % F: pid del processo friendship morto
        % _Reason: ragione della morte
        {'DOWN', _Ref, process, _F, _Reason} ->
            demonitor(_Ref, [flush]),

            Snew = spawn(?MODULE, state, []),
            Dnew = spawn(?MODULE, detect, []),
            Fnew = spawn_link(?MODULE, friendship, []),

            wellknown ! {replace_pid, {F, S}, {Fnew, Snew}},
            Fnew ! {pid, Snew, Dnew},
            Dnew ! {pid, Snew, Fnew},
            Snew ! {pid, Fnew, Dnew},

            loop(Fnew, Snew, Dnew);
        _ ->
            io:format("Car ~p received an unknown message~n", [self()]),
            loop(F, S, D)
    end.   

% FRIENDSHIP COMPONENT
friendship() ->
    receive {pid, S, D} ->
        link(S),
        link(D),
        friendship([], S, D)
    end.

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
% Friends -> Actual list of friends of caller car
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
    Grid = utils:init_grid(5, 5, sconosciuto),
    receive {pid, F, D} ->
        link(F),
        link(D),
        state(F, D, Grid)
    end.

notify_friends(F, State, X, Y) ->
    F ! {getOwnFriends, self()},
    receive
        {myFriends, Friends} -> [S ! {notifyStatus, {X, Y}, State} || {_, S} <- Friends]
    end.


% Take the information of the Grid's cells.
% When a new information is discoverd, send a message to the friends (state components)
% If the goal is occupied, change it
% F -> pid of friendship component
% D -> pid of detect component
% Grid -> grid of the ambient
state(F, D, Grid) ->
    receive
        {askTarget, D, X, Y, Ref} ->
            % TODO: find new target 
            D ! {newTarget, X, Y, Ref},
            state(F, D, Grid);
        % receive the state from Detect to update the Grid and notify the new state
        % if the newState is different from the old one, send a message to the friends
            % get from F the list of friends
            % send the message to friends
        {notifyStatus, X, Y, IsFree} -> 
            NewState = case IsFree of 
                true -> libero;
                false -> occupato
            end,

            OldState = utils:get_state(Grid, X, Y),      
            utils:set_state(Grid, X, Y, NewState),         
            case OldState =/= IsFree of
                true -> notify_friends(F, IsFree, X, Y)
            end,

            % TODO: UPDATE TARGET!!!
            %
            state(F, D, Grid)
    end.


% DETECT COMPONENT
detect() ->
    receive {pid, S, F} ->
        link(S),
        link(F),
        POS = {rand:uniform(5), rand:uniform(5)},% TODO: change this, real grid size
        detect(S, POS)
    end.

% If no target is set, ask for a new one to State
detect(S, {XP,YP}) -> 
    S ! {askTarget, self(), XP, YP, Ref = make_ref()},
    receive
        {newTarget, XT, YT, Ref} ->
            render ! {target, S, XT, YT},
            detect(S, {XP,YP}, {XT,YT})
    end.

detect(S, {XP,YP}, {XT,YT} ) ->
    {NEW_XP, NEW_YP} = move(XP, YP, XT, YT), 
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
                    % Car is parked and waiting 
                    Time = rand:uniform(5),
                    timer:sleep(Time * 1000),
                    % Car is leaving
                    ambient ! {leave, self(), NewRef},
                    render ! {parked, S, XP, YP, false},
                    detect(S, {XP,YP});
                false ->
                    detect(S, {XP,YP}, {XT, YT})
            end
    end.

% TODO: to be implemented 
move(XP, YP, XT, YT) ->
    {XP,YP}.

            
            

    