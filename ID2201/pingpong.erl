-module(pingpong).
-compile(export_all).


ping() ->
    {pong, "bar@192.168.0.121"} ! self(),

receive 
    input -> 
        io:format(input)
end.

pong() -> 
    receive 
        PID -> PID ! "pong!"
    end. 