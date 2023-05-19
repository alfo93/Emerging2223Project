-module(wellknown).
-export([main/1, create_friends_list/2]).

main(Pids) ->
    receive
        % register new pid
        {register_pid, {F, S}} ->         
            monitor(process, F),
            main([{F, S}|Pids]);
        % replace old pid with new pid
        {replace_pid, OldPid, NewPid} ->
            NewPids = lists:delete(OldPid, Pids),
            main([NewPid|NewPids]);
        % Send list of cars to another car
        {getFriends, F, S, Ref} ->
            % NewFriends -> car list permutation.
            NewFriends = create_friends_list(Pids, {F, S}),
            F ! {myFriends, NewFriends, Ref},
            main(Pids);
        {'DOWN', Ref, process, Pid, _Reason} ->
            demonitor(Ref, [flush]),
            Dead_Car = lists:keyfind(Pid, 1, Pids),
            main(lists:delete(Dead_Car, Pids));
        _ ->
            main(Pids)
    end.

% List permutation
permutation(X) ->
    [ Y || {_,Y} <- lists:sort([ {rand:uniform(), N} || N <- X]) ].

% FS: {F, S} -> { Friendship, State }
% List: [{F, S}*] 
create_friends_list(List, FS) ->
    X = lists:delete(FS, permutation(List)),
    lists:sublist(X, 5).
