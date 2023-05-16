-module(render).
-export([main/0]).
-include("utils.hrl").



% La funzione loop/0 rappresenta il ciclo di vita dell'attore render 
% dove ogni 2 secondi viene richiesta la griglia dell'ambient 
% e stampata a video tramite la funzione print_grid/0.

main() ->
    timer:send_after(5000, timeout),
    loop([], [], [], [], []).

loop(Pos, Targets, Parked, Friendships, Status) ->
    receive
        {position, PID, X, Y} -> 
            New_pos = lists:keydelete(PID, 1, Pos),
            loop(New_pos ++ [{PID,{X,Y}}], Targets, Parked, Friendships, Status);
        {target, PID, X, Y} ->
            NewTargets = lists:keydelete(PID, 1, Targets),
            loop(Pos, NewTargets ++ [{PID,{X,Y}}], Parked, Friendships, Status);
        {parked, PID, X,Y, IsParked} ->
            NewParked = lists:keydelete(PID, 1, Parked),
            loop(Pos, Targets, NewParked ++ [{PID, {X,Y,IsParked}}], Friendships, Status);
        {friendship, PID, PIDLIST} ->
            monitor(process, PID),
            NewFriendships = lists:keydelete(PID, 1, Friendships),
            loop(Pos, Targets, Parked, NewFriendships ++ [{PID,{PIDLIST}}], Status);
        {status, PID, Msg} ->
            NewMsg = lists:keydelete(PID, 1, Status),
            loop(Pos, Targets, Parked, Friendships, NewMsg ++ [{PID, Msg}]);
        {'DOWN', Ref,  process, PID, _Reason} ->
            demonitor(Ref, [flush]),
            NewPos = lists:keydelete(PID, 1, Pos),
            NewFriendships = lists:keydelete(PID, 1, Friendships),
            NewTargets = lists:keydelete(PID, 1, Targets),
            NewParked = lists:keydelete(PID, 1, Parked),
            NewStatus = lists:keydelete(PID, 1, Status),
            loop(NewPos, NewTargets, NewParked, NewFriendships, NewStatus);
        timeout ->
            utils:clear_screen(),
            print_grid(Pos),
            % count parked cars where IsParked = true
            Cars_n = length(Pos),
            Cars_parked = [IsParked || {_, {_, _, IsParked}} <- Parked, IsParked],
            Parked_car_n = length(Cars_parked),
            io:format("Cars: ~p, Parked: ~p, Moving: ~p~n", [Cars_n, Parked_car_n, Cars_n - Parked_car_n]), 
            print_car_info(Pos, Targets, Parked, Friendships, Status),
            timer:send_after(1000, timeout),
            loop(Pos, Targets, Parked, Friendships, Status);
        _ -> 
            loop(Pos, Targets, Parked, Friendships, Status)
    end.



print_grid(Pos) ->
    ambient ! get_grid,
    receive
        {grid, ParkingGrid} ->
            % io:format("Pos length: ~p~n", [length(Pos)]),
            %io:format("Pos: ~p~n", [Pos]),
            % For each Pos X,Y set the cell to "C"
            Grid_car = lists:foldl(fun({_PID, {X, Y}}, Grid) ->
                case utils:get_state(Grid, X, Y) of
                    libero -> utils:set_state(Grid, X, Y, macchina);
                    _ -> Grid
                end
            end, ParkingGrid, Pos),
            

            Grid = lists:map(fun(Row) ->lists:map(fun print_cell/1, Row) end, Grid_car),
            io:format("~n"),
            Width = length(hd(Grid)),
            io:format("~s~n", [string:copies("-", Width * 4 + 1)]),
            lists:foreach(fun(Row) ->
                lists:foreach(fun(Cell) ->
                    io:format("| ~s ", [Cell])
                end, Row),
                io:format("|\n"),
            io:format("~s~n", [string:copies("-", Width * 4 + 1)])
            end, Grid)
    end.

print_cell(libero) -> " ";
print_cell(occupato) -> "P";
print_cell(macchina) -> "C".

print_car_info([], _, _, _, _) -> io:format("-------------------------------------------------~n");
print_car_info([{PID, {X, Y}}|PIDS], Targets, Parked, Friendships, Status) -> 
    % Take PID target
    {_, {X_T, Y_T}} = lists:keyfind(PID, 1, Targets),
    {_, {_, _, IsParked}} = lists:keyfind(PID, 1, Parked),
    {_, {Friends}} = lists:keyfind(PID, 1, Friendships),
    Friends_to_print = lists:map(fun({_, P}) -> P end, Friends),
    io:format("\nPID: ~p, Pos: ~p, Target: ~p, Parked: ~p, \nFriends: ~p~n", [PID, {X, Y}, {X_T, Y_T}, IsParked, Friends_to_print]),
    io:format("Status: ~p~n", [lists:keyfind(PID, 1, Status)]),

    print_car_info(PIDS, Targets, Parked, Friendships, Status).

% Legenda
% " " = posto libero
% "P" = posto occupato da un'auto
% "C" = car
% "X" = Target
% Lista amici = PID: [PID1, PID2, ...]