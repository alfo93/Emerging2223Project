-module(utils).
-export([init_grid/1,init_grid/3, get_state/3, set_state/4, find_free_cell/1, clear_screen/0]).
-include("utils.hrl").

set_state(Grid, X, Y, NewState) ->
    case lists:nth(X, Grid) of
        Row when is_list(Row) ->
            case lists:nth(Y, Row) of
                State when State =:= libero; State =:= occupato; State =:= sconosciuto; State =:= macchina ->
                    NewRow = lists:sublist(Row, Y-1) ++ [NewState] ++ lists:nthtail(Y, Row),
                    lists:sublist(Grid, X-1) ++ [NewRow] ++ lists:nthtail(X, Grid);
                _ ->
                    Grid
            end
    end.

get_state(Grid, X, Y) ->
    case lists:nth(X, Grid) of
        Row when is_list(Row) -> lists:nth(Y, Row)
    end.

init_grid(State) ->
    init_grid(?GRID_WIDTH, ?GRID_HEIGHT, State).

init_grid(W, H, State) ->
    lists:map(fun(_) -> lists:map(fun(_) -> State end, lists:seq(1, H)) end, lists:seq(1, W)).


find_free_cell(Grid) ->
    TX = rand:uniform(?GRID_WIDTH),
    TY = rand:uniform(?GRID_HEIGHT),
    case get_state(Grid, TX, TY) =:= occupato of
        true ->
            find_free_cell(Grid);
        false ->
            {TX, TY}
    end.


clear_screen() ->
    io:format("~s\e[H\e[2J", ["\e"]).