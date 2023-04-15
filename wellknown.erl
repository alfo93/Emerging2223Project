-module(wellknown).
-export([main/1]).

main(Pids) ->
    receive
        {register_pid, Pid} ->
            main([Pid|Pids]);
        {remove_pid, Pid} ->
            main(lists:delete(Pid, Pids));
        {replace_pid, OldPid, NewPid} ->
            main(lists:replace(OldPid, NewPid, Pids));
        {getFriends, PID1, PID2, Ref} ->
            PID1 ! {myFriends, get_FriendsList(Pids, [], PID1, PID2), Ref},
            main(Pids);
        _ ->
            main(Pids)
    end.


get_FriendsList(Pids, FriendsList, P1, P2) ->
    case length(FriendsList) < 5 of
        true ->
            H = lists:nth(rand:uniform(length(Pids)), Pids),
            case H == [P1, P2] of
                true ->
                    get_FriendsList(Pids, FriendsList, P1, P2);
                false ->
                    get_FriendsList(Pids, [H | FriendsList], P1, P2)
            end;
        false ->
            FriendsList
    end.