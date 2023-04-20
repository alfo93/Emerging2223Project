-module(wellknown).
-export([main/1, create_FriendsList/4]).

main(Pids) ->
    receive
        {register_pid, Pid} ->
            main([Pid|Pids]);
        {remove_pid, Pid} ->
            main(lists:delete(Pid, Pids));
        {replace_pid, OldPid, NewPid} ->
            NewPids = lists:delete(OldPid, Pids),
            main([NewPid|NewPids]);
        {getFriends, PID1, PID2, Ref} ->
            NewFriends = create_FriendsList(Pids, PID1, PID2, []),
            PID1 ! {myFriends, lists:sublist(NewFriends, 5), Ref},
            main(Pids);
        _ ->
            main(Pids)
    end.

create_FriendsList([], _, _, FriendsList) ->
    FriendsList;

create_FriendsList(List, P1, P2, FriendsList) ->
    H = lists:nth(rand:uniform(length(List)), List),
    T = lists:delete(H, List),
    case H == [P1, P2] of
        true ->
            create_FriendsList(T, P1, P2, FriendsList);
        false ->
            create_FriendsList(T, P1, P2, [H|FriendsList])
    end.

    

