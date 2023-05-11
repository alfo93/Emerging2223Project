-module(utils).
-define(MAX_FRIENDS,5).
-define(GRID_WIDTH, 100).
-define(GRID_HEIGHT, 100).
-define(TIME_STEP, 2000).
-export([init_grid/3, get_state/3, set_state/4]).



set_state(Grid, X, Y, NewState) ->
    case lists:nth(X, Grid) of
        Row when is_list(Row) ->
            case lists:nth(Y, Row) of
                State when State =:= libero; State =:= occupato; State =:= sconosciuto ->
                    NewRow = lists:sublist(Row, Y-1) ++ [NewState] ++ lists:nthtail(Y, Row),
                    lists:sublist(Grid, X-1) ++ [NewRow] ++ lists:nthtail(X, Grid);
                _ ->
                    Grid
            end
    end.

get_state(Grid, X, Y) ->
    case lists:nth(X, Grid) of
        Row when is_list(Row) ->
            case lists:nth(Y, Row) of
                State when State =:= libero; State =:= occupato; State =:= sconosciuto ->
                    State
            end
    end.

init_grid(W, H, State) ->
    lists:map(fun(_) -> lists:map(fun(_) -> State end, lists:seq(1, H)) end, lists:seq(1, W)).