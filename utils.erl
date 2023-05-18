-module(utils).
-export([init_grid/1,init_grid/3, get_state/3, set_state/4, find_free_cell/1, clear_screen/0]).
-include("utils.hrl").

set_state(Grid, X, Y, NewState) ->
        Row = lists:nth(X, Grid),
        NewRow = lists:sublist(Row, Y-1) ++ [NewState] ++ lists:nthtail(Y, Row),
        lists:sublist(Grid, X-1) ++ [NewRow] ++ lists:nthtail(X, Grid).

get_state(Grid, X, Y) ->
        Row = lists:nth(X, Grid),
        lists:nth(Y, Row).

init_grid(State) -> init_grid(?GRID_WIDTH, ?GRID_HEIGHT, State).

init_grid(W, H, State) ->
    lists:duplicate(H, lists:duplicate(W, State)).

find_free_cell(Grid) ->
    TX = rand:uniform(?GRID_WIDTH),
    TY = rand:uniform(?GRID_HEIGHT),
    
    case get_state(Grid, TX, TY) of
        occupied -> find_free_cell(Grid);
        _ -> {TX, TY}
    end.

clear_screen() -> io:format("~s\e[H\e[2J", ["\e"]).