-module(ambient).
-export([main/0, car_killer/1, loop/1]).
-include("utils.hrl").

main() ->
    % Initializing grid.
    Grid = utils:init_grid(free),

    % Spawn `ambient` actor and register its atom.
    A = spawn(?MODULE, loop, [Grid]),
    register(ambient, A),
    
    % Spawn `wellknown` actor and register its atom.
    W = spawn(wellknown, main, [[]]),
    register(wellknown, W),

    % Spawn `N_CARS` cars.
    Cars = [spawn(car, main, []) || _ <- lists:seq(1, ?N_CARS)],
    
    % Spawn `render` actor and register its atom.
    R = spawn_link(render, main, []),
    register(render, R),
    render ! {render_status, whereis(ambient), "Ambient started."},
    
    car_killer(Cars).

car_killer(Cars) ->
    % Kill a random car every `KILLER_TIMEOUT` seconds.
    timer:sleep(?KILLER_TIMEOUT),
    Car = lists:nth(rand:uniform(length(Cars)), Cars),  
    exit(Car, die),

    NewCar = spawn_link(car, main, []),
    car_killer([NewCar|lists:delete(Car, Cars)]).

loop(Grid) ->
    % Avoid killing ambient even though linked procesess die.
    process_flag(trap_exit, true),
    loop(Grid, []). % ParkedCars = []

loop(Grid, ParkedCars) ->
    receive
        % Render the grid
        get_grid ->
            render ! {grid, Grid},
            loop(Grid,ParkedCars);
        
        {isFree, PID, X, Y, Ref} ->
            IsFree = case utils:get_state(Grid, X, Y) of
                free -> true;
                _ -> false
            end,
            PID ! {status, Ref, IsFree},
            loop(Grid, ParkedCars); 

        % Park it if the spot is free.
        {park, PID, X, Y, Ref} ->
            case utils:get_state(Grid, X, Y) of
                free ->
                    Mref = monitor(process, PID),
                    NewGrid = utils:set_state(Grid, X, Y, occupied),
                    render ! {render_status, self(), {"Car " ,PID, " parked in ", {X,Y}}},
                    loop(NewGrid, ParkedCars ++ [{PID,X,Y, Ref, Mref}]);
                _ ->
                    % Handle the case where the spot is not free. 
                    render ! {render_status, self(), {"Car ",PID," cannot park in ",{X,Y} ," Killing it."}},
                    exit(PID, park_full),
                    loop(Grid, ParkedCars)
            end;

        % Leave the park it if it is parked.
        {leave, PID, Ref} ->
            case lists:keyfind(PID, 1, ParkedCars) of
                % Match case with same Ref
                {PID, X, Y, Ref, Mref} -> 
                        NewGrid = utils:set_state(Grid, X, Y, free),
                        demonitor(Mref),
                        render ! {render_status, self(), {"Car ", PID, " left ",{X, Y}}},
                        loop(NewGrid, lists:keydelete(PID, 1, ParkedCars));
                _ ->
                        exit(PID, not_parked),
                        loop(Grid, ParkedCars)
                end;
            
        % Handle the case where a parked car dies.
        {'DOWN', _, process, PID, _} ->
            case lists:keyfind(PID, 1, ParkedCars) of
                false ->
                    % Handle the case where PID is not found in ParkedCars
                    loop(Grid, ParkedCars);
                {_, X, Y, _Ref, Mref} ->
                    demonitor(Mref),
                    NewGrid = utils:set_state(Grid, X, Y, free),
                    loop(NewGrid, lists:keydelete(PID, 1, ParkedCars))
            end
    end.





