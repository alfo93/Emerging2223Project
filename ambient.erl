-module(ambient).
-export([main/0, loop/2]).

% Nella funzione main/0 viene creato il processo dell'attore ambient e viene registrato con il nome ambient. 
% La funzione loop/1 rappresenta il ciclo di vita dell'attore ambient e riceve messaggi tramite il costrutto receive.
% L'attore risponde a due tipi di messaggi:
%    {check_state, Caller, {X, Y}}: richiesta di stato per la cella alle coordinate (X,Y). L'attore risponde inviando un messaggio {state, State} al chiamante Caller.
%    {update_state, {X, Y}, NewState}: richiesta di aggiornamento dello stato per la cella alle coordinate (X,Y). L'attore aggiorna la griglia dell'ambient e continua il ciclo.
%    get_grid: richiesta di stampa della griglia dell'ambient. L'attore invia un messaggio contenente la griglia al processo render.
% La funzione get_state/3 restituisce lo stato corrente della cella nella posizione (X,Y) nella griglia. 
% La funzione set_state/4 aggiorna lo stato della cella nella posizione (X,Y) nella griglia con il nuovo stato NewState.
% La griglia dell'ambient è rappresentata come una lista di liste in cui ogni elemento rappresenta lo stato di una cella (libero o occupato).

main() ->
    Grid = utils:init_grid(5, 5, libero),
    A = spawn(?MODULE, loop, [Grid, []]),
    register(ambient, A),
    W = spawn(wellknown, main, [[]]),
    register(wellknown, W),

    Cars = [spawn(car, main, []) || _ <- lists:seq(1, 6)],

    R = spawn_link(render, main, []),
    register(render, R),
    car_killer(Cars).

car_killer(Cars) ->
    timer:sleep(2000),
    Car = lists:nth(rand:uniform(length(Cars)), Cars),
    io:format("Car~p is going to die!~n", [Car]),
    exit(Car, die),
    NewCar = spawn(car, main, []),
    io:format("A new car ~p is born!~n", [NewCar]),
    car_killer([NewCar|lists:delete(Car, Cars)]).

loop(Grid, ParkedCars) ->
    receive
        {check_state, Caller, {X, Y}} ->
            State = utils:get_state(Grid, X, Y),
            Caller ! {state, State},
            loop(Grid,ParkedCars);
        {update_state, {X, Y}, NewState} ->
            NewGrid = utils:set_state(Grid, X, Y, NewState),
            loop(NewGrid, ParkedCars);
        get_grid ->
            render ! Grid,
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
            Mref = monitor(process, PID),
            NewGrid = utils:set_state(Grid, X, Y, occupato),
            loop(NewGrid,ParkedCars ++ [{PID,X,Y, Ref, Mref}]);

        {leave, PID, Ref} ->
            case lists:keyfind(PID, 1, ParkedCars) of
                {PID, X, Y, OldRef, Mref} when Ref =:= OldRef -> 
                        io:format("Car ~p is leaving in ~p, ~p~n", [PID, X, Y]),
                        NewGrid = utils:set_state(Grid, X, Y, libero),
                        demonitor(Mref),
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





