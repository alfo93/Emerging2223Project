-module(wellknown).
-export([main/1, create_friends_list/2]).

main(Pids) ->
    receive
        % aggiungo una macchina (data dalla coppia FS) alla lista di macchine
        {register_pid, Pid} ->
            main([Pid|Pids]);
        % rimuovo una macchina dalla lista di macchine
        {remove_pid, Pid} ->
            main(lists:delete(Pid, Pids));
        % sostituisco una macchina con un altra nel caso in cui sia crashata
        {replace_pid, OldPid, NewPid} ->
            NewPids = lists:delete(OldPid, Pids),
            main([NewPid|NewPids]);
        % invio la lista di macchine ad una macchina
        {getFriends, F, S, Ref} ->
            % NewFriends -> permutazione della lista di tutte le macchine.
            NewFriends = create_friends_list(Pids, {F, S}),
            F ! {myFriends, NewFriends, Ref},
            main(Pids);
        _ ->
            main(Pids)
    end.



% permutazione della lista di macchine
permutation(X) ->
    [Y||{_,Y} <- lists:sort([ {rand:uniform(), N} || N <- X])].

% FS: {F, S} -> { Friendship, State }
% List: [{F, S}*] 
create_friends_list(List, FS) ->
    X = lists:delete(FS, permutation(List)),
    lists:sublist(X, 5).
