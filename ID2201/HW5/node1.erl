-module(node1).
-compile(export_all).
-define(Stabilize, 1000).
-define(Timeout, 10000).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor).

connect(Id, nil) ->
    {ok, {Id, self()}};
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, Peer}}
    after ?Timeout ->
        io:format("Time out: no response~n",[])
    end.

request(Peer, Predecessor) ->
    case Predecessor of 
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end. 


notify({Nkey, Npid}, Id, Predecessor) -> %do we need a special case to detect that we're pointing to ourselves? No, cuz if NKEY=PKEY we just give true
    case Predecessor of 
        nil ->
            {Nkey, Npid};
        
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of %om Nkey ligger inom Pkey och Id, så adopterar 
                true ->
                    {Nkey, Npid};
                false ->
                    Predecessor
            end
        end. 

node(Id, Predecessor, Successor) ->
    receive 
        {key, Qref, Peer} ->
            Peer ! {Qref, Id}, 
            node(Id, Predecessor, Successor);
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);

        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor);

        probe ->

            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);

        {probe, Id, Nodes, T} ->

            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);

        {probe, Ref, Nodes, T} ->
            % io:format("am I here lol", []),
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor)
    end. 

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).


stabilize({_, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            Spid ! {notify, {Id, self()}},
            Successor;

        {Id, _} ->
            Successor;

        {Skey, _} ->
            Spid ! {notify, {Id, self()}},
            Successor;

        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of %om xkey ligger inom Id och Skey betyder det att vi vill adoptera. Vi tittar på det som ligger närmre oss.
                true ->
                    Xpid ! {request, self()},
                    Pred;% {Xkey, Xpid};                
                false ->
                    Spid ! {notify, {Id, self()}}, %if there is an empty space, we notify that we're there. 
                    Successor
            end
    end.



create_probe(Id, {_, Spid}) ->
    T = erlang:system_time(micro_seconds),
    Spid ! {probe, Id, [], T}.


forward_probe(Ref, T, Nodes, Id, {_, Spid}) ->
    io:format("forwarding: ~w~n", [Id]),
    % Spid ! {probe, Ref, [Id | Nodes], T}.
  Spid ! {probe, Ref, lists:append(Nodes, [Id]), T}.


remove_probe(T, Nodes) ->
    Time = erlang:system_time(micro_seconds),
    Timediff = Time - T,
    io:format("Timetotal: ~w~nNodes, ~w~n", [Timediff, Nodes]).






