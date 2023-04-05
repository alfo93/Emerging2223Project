-module(car).
-export([main/0]).

main() ->
    spawn_link(?MODULE, friendship, []),
    spawn_link(?MODULE, detect, []),
    spawn_link(?MODULE, state, []),
    loop().

loop() ->
    receive
        {'EXIT', _Friendship, _Reason} ->
            io:format("Friendship actor crashed!~n"),
            spawn_link(?MODULE, friendship, []),
            loop();
        {'EXIT', _State, _Reason} ->
            io:format("State actor crashed!~n"),
            spawn_link(?MODULE, state, []),
            loop();
        {'EXIT', _Detect, _Reason} ->
            io:format("Detect actor crashed!~n"),
            spawn_link(?MODULE, detect, []),
            loop()
    end.

friendship() ->
    pass.

state() ->
    pass.

detect() ->
    pass.









