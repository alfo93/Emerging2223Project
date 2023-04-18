-module(car).
-export([main/0, loop/3, friendship/2, state/0, detect/0]).


main() ->
    {S, _} = spawn_monitor(?MODULE, state, []),
    {D, _} = spawn_monitor(?MODULE, detect, []),
    {F, _} = spawn_monitor(?MODULE, friendship, [[], S]),

    wellknown ! {register_pid, [F, S]},
    loop(F, S, D).

loop(F, S, D) ->
    receive
        {'DOWN', _Ref, process, Pid, _Reason} ->
            case Pid of
                F -> 
                    io:format("Friendship process died~n"),
                    {NewPid, _} = spawn_monitor(fun () -> friendship([],S) end),
                    wellknown ! {replace_pid, [F, S], {NewPid, S}},
                    loop(NewPid, S, D);
                S -> 
                    io:format("State process died~n"),
                    {NewPid, _} = spawn_monitor(fun state/0),
                    wellknown ! {replace_pid, [F, S], {F, NewPid}},
                    loop(F, NewPid, D);
                D -> 
                    io:format("Detect process died~n"),
                    {NewPid, _} = spawn_monitor(fun detect/0),
                    loop(F, S, NewPid);
                _ ->
                    io:format("Unknown process~n with pid: ~p~n", [Pid]),
                    loop(F, S, D)
            end;
        _ ->
            io:format("Unknown message~n"),
            loop(F, S, D)
    end.   


friendship(Friends, MyState) ->
    % io:format("Car: ~p, Friends: ~p~n", [self(), Friends]),
    case length(Friends) < 5 of
        true ->
            friendship(ask_for_friends(MyState, Friends, []), MyState);
        false ->
            receive
                {getFriends, PID1, PID2, Ref} -> 
                    NewFriends = lists:delete([PID1,PID2], Friends),
                    PID1 ! {myFriends, NewFriends, Ref},
                    case length(NewFriends) < 5 of
                        true ->
                            friendship([[PID1,PID2] | NewFriends], MyState);
                        false ->
                            friendship(NewFriends, MyState)
                    end;
                    
                _ ->
                    io:format("Unknown message~n"),
                    friendship(Friends, MyState)
            end
    end.
    


state() ->
    % sleep for 10 seconds
    timer:sleep(200000),
    io:format("State bye bye~n").

detect() ->
    % sleep for 15 seconds
    timer:sleep(200000),
    io:format("Detect bye bye~n").



% S -> State process of caller car
% Friends -> Friends of caller car
% NewFriendsList -> List of new friends
ask_for_friends(S, [], FriendsList) ->
    wellknown ! {getFriends, self(), S, Ref = make_ref()},
        receive
            {myFriends, NewFriends, Ref} ->
                NewFriendsList = lists:usort(FriendsList ++ NewFriends),

                case length(NewFriendsList) < 5 of
                    true ->
                        ask_for_friends(S, NewFriendsList, []);
                    false ->
                        NewFriendsList
                end
        end;
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
                    NewFriendsList
            end
        % Can be useful?
        % _ ->
        %     io:format("Unknown message"),
        %     ask_for_friends(S, [F | Fs], FriendsList)
    end.