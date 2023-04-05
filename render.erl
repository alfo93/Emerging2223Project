-module(render).
-export([main/0]).

% La funzione loop/0 rappresenta il ciclo di vita dell'attore render dove ogni 2 secondi viene richiesta la griglia dell'ambient e stampata a video tramite la funzione print_grid/0.


main()->
    loop().

print_grid() ->
    ambient ! get_grid,
    receive
        Grid ->
            io:format("~p~n", [Grid])
    end.

loop() ->
    timer:sleep(2000),
    print_grid(),
    loop().