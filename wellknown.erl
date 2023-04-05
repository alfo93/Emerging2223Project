-module(erlang_actor).
-export([main/1]).

main(Pids) ->
    receive
        {register_pid, Pid} ->
            main([Pid|Pids]);
        {get_pids, From} ->
            From ! {pids, Pids},
            main(Pids);
        _ ->
            main(Pids)
    end.