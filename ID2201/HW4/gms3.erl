-module(gms3).
-compile(export_all).
-define(timeout, 200).
-define(arghh, 100).


start(Id) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

start(Id, Grp) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.
 
init(Id, Rnd, Master)->
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, [], [Master], 1).

init(Id, Rnd, Grp, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    Self = self(), 
    Grp ! {join, Master, Self},
    receive
        {view, N, [Leader|Slaves], Group} ->
            erlang:monitor(process, Leader),
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group, N+1, {view, N, [Leader|Slaves], Group})
    after ?timeout ->
                Master ! {error, "no reply from leader"}
    end. 

leader(Id, Master, Slaves, Group, N) ->
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, N, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, Slaves, Group, N+1);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2, N+1);
        stop ->
            ok
    end. 

slave(Id, Master, Leader, Slaves, Group, N, Last) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group, N, Last);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group, N, Last);
        {msg, N2, Msg} ->
            case N2 < N of %om vi får ett gammalt meddelande, gå vidare, annars kalla med det nya meddelandet och inkrementa
                true ->
                    slave(Id, Master, Leader, Slaves, Group, N, Last);
                false ->
                    Master ! Msg,
                    slave(Id, Master, Leader, Slaves, Group, N+1, {msg, N2, Msg})
                end;

        {view, N2, [Leader|Slaves2], Group2} ->
           
            case N2 < N of
                true ->
                    slave(Id, Master, Leader, Slaves, Group, N, Last);
                false->
                     Master ! {view, Group2},
                    slave(Id, Master, Leader, Slaves2, Group2, N2+1, {view, N2, [Leader|Slaves2]})
                end;

        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, Slaves, Group, N, Last);
        stop ->
            ok
end. 






bcast(Id, Msg, Nodes) -> %anledningen tlll att det blir out of sync vid en crash är att broadcasten går igenom en lista med noder och säger till dem att broadcasta. Vid varje broadcast har vi implementerat en random crash chans, om den crashar kommer den noden missa ett meddelande och därmed blir de out of sync.
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
    case random:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash~n", [Id]),
            exit(no_luck);
        _ ->
            ok
    end. 


election(Id, Master, Slaves, [_|Group], N, Last) ->
    Self = self(), 
    case Slaves of
        [Self|Rest] ->
            bcast(Id, Last, Rest),
            bcast(Id, {view, N, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, Rest, Group, N+1);
        [Leader|Rest] ->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, Rest, Group, N, Last)
    end. 



