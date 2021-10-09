-module(time).
-compile(export_all).

zero() ->
    0. 

inc(Name, T) ->
    T+1.

merge(Ti, Tj) ->
 

    max(Ti, Tj).

leq(Ti, Tj) ->
    if 
        Ti > Tj ->
            false;
        Ti =< Tj ->
            true 
        end.


clock(Nodes) ->
        lists:map(fun (Node) -> {Node, zero()} end, Nodes).


update(Node, Time, Clock) ->
	UpdatedClock = lists:keyreplace(Node, 1, Clock, {Node, Time}),
    lists:keysort(2, UpdatedClock).

safe(Time, [{Name, OldTime}|List]) ->
    leq(Time, OldTime). 

