-module(wait).
-compile(export_all).

hello() ->
    receive
        X -> io:format("aaa! suprise, a message: ~s~n", [X])
    end.

area(X, Y) -> X*Y.
area(X) -> X*10.
