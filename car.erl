-module(car).
-export([main/0, loop/3, friendship/0, state/0, detect/0]).

main() ->
    {F, _} = spawn_monitor(fun () -> friendship() end),
    {S, _} = spawn_monitor(fun () -> state() end),
    {D, _} = spawn_monitor(fun () -> detect() end),

    loop(F, S, D).

loop(F, S, D) ->
    receive
        {'DOWN', _Ref, process, Pid, _Reason} ->
            case Pid of
                F -> 
                    io:format("Friendship process died~n"),
                    {NewPid, _} = spawn_monitor(fun friendship/0),
                    loop(NewPid, S, D);
                S -> 
                    io:format("State process died~n"),
                    {NewPid, _} = spawn_monitor(fun state/0),
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

friendship() ->
    % sleep for 5 seconds
    timer:sleep(1000),  
    1/0,
    io:format("Friendship bye bye~n").

state() ->
    % sleep for 10 seconds
    timer:sleep(20000),
    io:format("State bye bye~n").

detect() ->
    % sleep for 15 seconds
    timer:sleep(50000),
    io:format("Detect bye bye~n").










