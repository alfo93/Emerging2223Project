-module(car).
-export([main/0, friendship/0, state/0, detect/0]).


main() ->
    S = spawn(?MODULE, state, []),
    D = spawn(?MODULE, detect, []),
    {F, _} = spawn_monitor(?MODULE, friendship, []),

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
            Snew = spawn(?MODULE, state, []),
            Dnew = spawn(?MODULE, detect, []),
            {Fnew, _} = spawn_monitor(?MODULE, friendship, []),

            wellknown ! {replace_pid, [F, S], [Fnew, Snew]},
            Fnew ! {pid, Snew, Dnew},
            Dnew ! {pid, Snew, Fnew},
            Snew ! {pid, Fnew, Dnew},

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
    case length(Friends) < 5 of
        true ->
            io:format(" Car ~p is searching for new friends, now has ~p~n", [self(), length(Friends)]),
            NewFriends = ask_for_friends(MyState, Friends),
            render ! {friendship, MyState, NewFriends},
            friendship(NewFriends, MyState, MyDetect);
        false ->
            io:format(" Car ~p has enough friends, now has ~p~n", [self(), length(Friends)]),
            receive
                {getFriends, PID1, PID2, Ref} -> 
                    io:format(" Car ~p has received a request for friends from ~p~n", [self(), PID1]),
                    NewFriends = lists:delete([PID1,PID2], Friends),
                    PID1 ! {myFriends, NewFriends, Ref},
                    case length(NewFriends) < 5 of
                        true ->
                            friendship([[PID1,PID2] | NewFriends], MyState, MyDetect);
                        false ->
                            friendship(NewFriends, MyState, MyDetect)
                    end
            end
    end.
    

% S -> State process of caller car
% Friends -> Actual list of friends of caller car
ask_for_friends(S,FriendsList) ->
    ask_for_friends(S, FriendsList, FriendsList).

% S -> State process of caller car
% Friends -> Friends of caller car
% NewFriendsList -> List of new friends

ask_for_friends(S, [F | Fs], FriendsList) ->
    % Ask to Friends
    [Pid_F, _] = F,
    Pid_F ! {getFriends, self(), S, Ref = make_ref()},
    io:format("Car ~p is waiting for response from car ~p~n", [self(), Pid_F]),
    wait_for_response(S, [F | Fs], FriendsList, Ref);

% If the list is empty, search for new friends through wellknown actor
ask_for_friends(S, [], FriendsList) ->
    wellknown ! {getFriends, self(), S, Ref = make_ref()},
    io:format("Car ~p is searching for new friends through wellknown actor, now has ~p~n", [self(), length(FriendsList)]),
    wait_for_response(S, FriendsList, FriendsList, Ref).


wait_for_response(S, F, FriendsList, Ref) ->
    receive
        {getFriends, PID1, PID2, Ref1} -> 
            io:format(" Car ~p has received a request for friends from ~p~n", [self(), PID1]),
            NewFriends = lists:delete([PID1,PID2], FriendsList),
            PID1 ! {myFriends, NewFriends, Ref1},
            wait_for_response(S,F,FriendsList,Ref);
        {myFriends, NewFriends, Ref} ->
            NewFriendsList = lists:usort(FriendsList ++ NewFriends),
            case length(NewFriendsList) < 5 of
                true ->
                    ask_for_friends(S, F, NewFriendsList);
                false ->
                    NewFriendsList
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