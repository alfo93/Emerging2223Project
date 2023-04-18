-module(render).
-export([main/0]).

% La funzione loop/0 rappresenta il ciclo di vita dell'attore render dove ogni 2 secondi viene richiesta la griglia dell'ambient e stampata a video tramite la funzione print_grid/0.

main() ->
    loop(#{}, #{}, #{}, #{}).

loop(Pos, Targets, Parked, Friendships) ->
    receive
        {position, PID, X, Y} -> 
            % Pos = maps:put(PID, {X, Y}, Pos),
            loop(Pos#{PID=>{X,Y}}, Targets, Parked, Friendships);
        {target, PID, X, Y} ->
            % Targets = maps:put(PID, {X, Y}, Targets),
            loop(Pos, Targets#{PID=>{X,Y}}, Parked, Friendships);
        {parked, PID, X,Y, IsParked} ->
            % Parked = maps:put(PID, {X, Y, IsParked}, Parked),
            loop(Pos, Targets, Parked#{PID=>{X,Y,IsParked}}, Friendships);
        {friendship, PID, PIDLIST} ->
            % Friendships = maps:put(PID, PIDLIST, Friendships),
            loop(Pos, Targets, Parked, Friendships#{PID=>PIDLIST});
        _ ->
            loop(Pos, Targets, Parked, Friendships)
    after 2000 ->
        % Print debugging informations
        print_grid(),
        loop(Pos, Targets, Parked, Friendships)
    end.


print_grid() ->
    ambient ! get_grid,
    receive
        ParkingGrid ->
            % map each element of the grid to a character
            Grid = lists:map(fun(Row) ->
                lists:map(fun print_cell/1, Row)
            end, ParkingGrid),
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
print_cell(occupato) -> "X".