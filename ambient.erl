-module(ambient).
-export([main/0, car_killer/1, loop/2]).
-include("utils.hrl").

main() ->
    Grid = utils:init_grid(libero),
    A = spawn(?MODULE, loop, [Grid, []]),

    register(ambient, A),

    W = spawn(wellknown, main, [[]]),
    register(wellknown, W),

    Cars = [spawn_link(car, main, []) || _ <- lists:seq(1, ?N_CARS)],
    
    R = spawn_link(render, main, []),
    register(render, R),
    render ! {render_status, ambient, "Ambient started."},
    
    car_killer(Cars).

car_killer(Cars) ->
    timer:sleep(10000),
    Car = lists:nth(rand:uniform(length(Cars)), Cars),
    % io:format("Killing car ~p~n", [Car]),
    exit(Car, die),
    % io:format("Respawning car ~p~n", [Car]),
    NewCar = spawn(car, main, []),
    car_killer([NewCar|lists:delete(Car, Cars)]).

loop(Grid, ParkedCars) ->
    receive
        {check_state, Caller, {X, Y}} ->
            State = utils:get_state(Grid, X, Y),
            Caller ! {state, State},
            loop(Grid,ParkedCars);
        get_grid ->
            render ! {grid, Grid},
            loop(Grid,ParkedCars);

        % Specifiche progetto 
        {isFree, PID, X, Y, Ref} ->
            IsFree = case utils:get_state(Grid, X, Y) of
                libero -> true;
                _ -> false
            end,
            PID ! {status, Ref, IsFree},
            loop(Grid, ParkedCars); 

        {park, PID, X, Y, Ref} ->
            case utils:get_state(Grid, X, Y) of
                libero ->
                    Mref = monitor(process, PID),
                    NewGrid = utils:set_state(Grid, X, Y, occupato),
                    render ! {render_status, ambient, {"Car " ,PID, " parked in ", {X,Y}}},
                    loop(NewGrid, ParkedCars ++ [{PID,X,Y, Ref, Mref}]);
                _ ->
                    render ! {render_status, ambient, {"Car ",PID," cannot park in ",{X,Y} ," Killing it."}},
                    exit(PID, full),
                    loop(Grid, ParkedCars)
            end;

        {leave, PID, Ref} ->
            case lists:keyfind(PID, 1, ParkedCars) of
                {PID, X, Y, OldRef, Mref} when Ref =:= OldRef -> 
                        NewGrid = utils:set_state(Grid, X, Y, libero),
                        demonitor(Mref),
                        render ! {render_status, ambient, {"Car ", PID, " left ",{X, Y}}},
                        loop(NewGrid, lists:keydelete(PID, 1, ParkedCars));
                    _ ->
                        % Error! PID not found or Ref is not the same, define behaviour
                        loop(Grid, ParkedCars)
                end;
            
        {'DOWN', _, process, PID, _} ->
            case lists:keyfind(PID, 1, ParkedCars) of
                false ->
                    % Handle the case where PID is not found in ParkedCars
                    loop(Grid, ParkedCars);
                {_, X, Y, _Ref, Mref} ->
                    demonitor(Mref),
                    NewGrid = utils:set_state(Grid, X, Y, libero),
                    loop(NewGrid, lists:keydelete(PID, 1, ParkedCars))
            end
    end.





