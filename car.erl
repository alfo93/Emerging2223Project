-module(car).
-export([main/0, friendship/0, state/0, detect/0]).


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
        {'DOWN', _Ref, process, F, _Reason} ->
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
    case length(Friends) < 5 of
        true ->
            AllMyFriends = ask_for_friends(MyState, Friends),            
            % monitoring all new friends
            [ monitor(process, F) || {F, _} <-lists:subtract(AllMyFriends, Friends)],
            render ! {friendship, MyState, AllMyFriends},
            friendship(AllMyFriends, MyState, MyDetect);
        false ->
            receive
                {getFriends, PID1, PID2, Ref} -> 
                    NewFriends = lists:delete({PID1,PID2}, Friends),
                    PID1 ! {myFriends, NewFriends, Ref},
                    friendship(Friends, MyState, MyDetect);
                {'DOWN', _Ref, process, _Pid, _Reason} ->
                    io:format("Car ~p has lost a friend~n", [self()]),
                    friendship(remove_dead_friend(Friends, _Pid), MyState, MyDetect)
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
    io:format("Car ~p is searching for new friends through wellknown actor, now has ~p~n", [self(), length(FriendsList)]),
    wait_for_response(S, FriendsList, FriendsList, Ref).


wait_for_response(S, F, FriendsList, Ref) ->
    receive
        {getFriends, PID1, PID2, NewRef} -> 
            io:format("Car ~p received a getFriends message from ~p~n", [self(), PID1]),
            NewFriends = lists:delete({PID1,PID2}, FriendsList),
            PID1 ! {myFriends, NewFriends, NewRef},
            MyNewFriends = lists:usort(FriendsList ++ NewFriends),
            case length(MyNewFriends) < 5 of
                true ->
                    ask_for_friends(S, F, MyNewFriends);
                false ->
                    MyNewFriends
            end;
        {myFriends, NewFriends, Ref} ->
            NewFriendsList = lists:usort(FriendsList ++ NewFriends),
            case length(NewFriendsList) < 5 of
                true ->
                    ask_for_friends(S, F, NewFriendsList);
                false ->
                    NewFriendsList
            end;
        {'DOWN', _Ref, process, _Pid, _Reason} ->
                    io:format("Car ~p has lost a friend~n", [self()]),
                    wait_for_response(S, remove_dead_friend(F, _Pid), remove_dead_friend(FriendsList, _Pid), Ref)
    end.


% remove_dead_friend: (list, pid) -> list
% rimuove dalla lista di amici 
% la coppia con il pid del processo morto
remove_dead_friend([], _) -> [];
remove_dead_friend([{F, S}|T], DeadFriend) -> 
    if F == DeadFriend -> T; 
        true -> [{F, S}|remove_dead_friend(T, DeadFriend)]
    end.




% STATE COMPONENT
state() ->
    receive {pid, F, D} ->
        link(F),
        link(D),
        state(F, D)
    end.

% TODO: implement this function
state(F, D) ->
    timer:sleep(200000000000),
    io:format("State bye bye~n").





% DETECT COMPONENT
detect() ->
    receive {pid, S, F} ->
        link(S),
        link(F),
        detect(S, F)
    end.

% TODO: implement this function
detect(S, F) ->
    timer:sleep(2000000000),
    io:format("Detect bye bye~n").