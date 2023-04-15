-module(ambient).
-export([main/0, loop/1]).

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
    Grid = init_grid(5, 5),
    A = spawn(?MODULE, loop, [Grid]),
    register(ambient, A),
    W = spawn(wellknown, main, [[]]),
    register(wellknown, W),

    Cars = [spawn(car, main, []) || _ <- lists:seq(1, 10)],

    R = spawn_link(render, main, []),
    register(render, R).

loop(Grid) ->
    receive
        {check_state, Caller, {X, Y}} ->
            State = get_state(Grid, X, Y),
            Caller ! {state, State},
            loop(Grid);
        {update_state, {X, Y}, NewState} ->
            NewGrid = set_state(Grid, X, Y, NewState),
            loop(NewGrid);
        get_grid ->
            render ! Grid,
            loop(Grid)
    end.

get_state(Grid, X, Y) ->
    case lists:nth(X, Grid) of
        Row when is_list(Row) ->
            case lists:nth(Y, Row) of
                State when State =:= libero; State =:= occupato ->
                    State
            end
    end.

set_state(Grid, X, Y, NewState) ->
    case lists:nth(X, Grid) of
        Row when is_list(Row) ->
            case lists:nth(Y, Row) of
                State when State =:= libero; State =:= occupato ->
                    NewRow = lists:sublist(Row, Y-1) ++ [NewState] ++ lists:nthtail(Y, Row),
                    lists:sublist(Grid, X-1) ++ [NewRow] ++ lists:nthtail(X, Grid)
            end
    end.

init_grid(W, H) ->
    lists:map(fun(_) -> lists:map(fun(_) -> libero end, lists:seq(1, H)) end, lists:seq(1, W)).