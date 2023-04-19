-module(car).
-export([main/0, loop/3, friendship/2, state/0, detect/0]).


main() ->
    {S, _} = spawn_monitor(?MODULE, state, []),
    {D, _} = spawn_monitor(?MODULE, detect, []),
    {F, _} = spawn_monitor(?MODULE, friendship, [[], S]),

    wellknown ! {register_pid, [F, S]},
    F ! {pid, S, D},
    D ! {pid, S, F},
    S ! {pid, F, D},

    loop(F, S, D).

loop(F, S, D) ->
    receive
        % {'DOWN', _Ref, process, F, _Reason} ->
        %     io:format("Friendship process died~n"),
        %     {NewPid, _} = spawn_monitor(fun () -> friendship([],S) end),
        %     wellknown ! {replace_pid, [F, S], {NewPid, S}},
        %     loop(NewPid, S, D);
        % {'DOWN', _Ref, process, S, _Reason} ->
        %     io:format("State process died~n"),
        %     {NewPid, _} = spawn_monitor(fun state/0),
        %     wellknown ! {replace_pid, [F, S], {F, NewPid}},
        %     loop(F, NewPid, D);
        % {'DOWN', _Ref, process, D, _Reason} ->
        %     io:format("Detect process died~n"),
        %     {NewPid, _} = spawn_monitor(fun detect/0),
        %     loop(F, S, NewPid);
        {'DOWN', _Ref, process, _Pid, _Reason} ->
            {Snew, _} = spawn_monitor(?MODULE, state, []),
            {Dnew, _} = spawn_monitor(?MODULE, detect, []),
            {Fnew, _} = spawn_monitor(?MODULE, friendship, [[], S]),
            wellknown ! {replace_pid, [F, S], {Fnew, Snew}},
            loop(Fnew, Snew, Dnew);
        _ ->
            io:format("Unknown message~n"),
            loop(F, S, D)
    end.   

% link tra le componenti della macchina
friendship() ->
    receive {pid, S, D} ->
        link(S),
        link(D),
        friendship([], S, D)
    end.

friendship(Friends, MyState, MyDetect) ->
    % io:format("Car: ~p, Friends: ~p~n", [self(), Friends]),
    case length(Friends) < 5 of
        true ->
            friendship(ask_for_friends(MyState, Friends), MyState, MyDetect);
        false ->
            receive
                {getFriends, PID1, PID2, Ref} -> 
                    NewFriends = lists:delete([PID1,PID2], Friends),
                    PID1 ! {myFriends, NewFriends, Ref},
                    case length(NewFriends) < 5 of
                        true ->
                            friendship([[PID1,PID2] | NewFriends], MyState, MyDetect);
                        false ->
                            friendship(NewFriends, MyState, MyDetect)
                    end;
                    
                _ ->
                    io:format("Unknown message~n"),
                    friendship(Friends, MyState, MyDetect)
            end
    end.
    


state() ->
    receive {pid, F, D} ->
        link(F),
        link(D),
        state(F, D)
    end.

state(F, D) ->
    timer:sleep(200000000000),
    io:format("State bye bye~n").




detect() ->
    receive {pid, S, F} ->
        link(S),
        link(F),
        detect(S, F)
    end.

detect(S, F) ->
    timer:sleep(2000000000),
    io:format("Detect bye bye~n").


ask_for_friends(S,FriendsList) ->
    ask_for_friends(S, FriendsList, FriendsList).

% S -> State process of caller car
% Friends -> Friends of caller car
% NewFriendsList -> List of new friends


ask_for_friends(S, [F | Fs], FriendsList) ->
    % Ask to Friends
    [Pid_F, _] = F,
    Pid_F ! {getFriends, self(), S, Ref = make_ref()},

    receive
        {myFriends, NewFriends, Ref} ->
            NewFriendsList = lists:usort(FriendsList ++ NewFriends),

            case length(NewFriendsList) < 5 of
                true ->
                    ask_for_friends(S, Fs, NewFriendsList);
                false ->
                    lists:sublist(NewFriendsList, 5)
            end
        % Can be useful?
        % _ ->
        %     io:format("Unknown message"),
        %     ask_for_friends(S, [F | Fs], FriendsList)
    end;

% If the list is empty, search for new friends through wellknown actor
ask_for_friends(S, [], FriendsList) ->
    wellknown ! {getFriends, self(), S, Ref = make_ref()},
    
    receive
        {myFriends, NewFriends, Ref} ->
            NewFriendsList = lists:usort(FriendsList ++ NewFriends),

            case length(NewFriendsList) < 5 of
                true ->
                    ask_for_friends(S, NewFriendsList, []);
                false ->
                    lists:sublist(NewFriendsList, 5)
            end
    end.