-module(node1).
-compile(export_all).
-define(Stabilize, 100).
-define(Timeout, 1000000).

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
      io:format("Time out: no response ~n", [])
    end.

request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
    end.


  % being notified of a node is a way for a node to make
  % a friendly proposal that it might be our proper predecessor
  notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
      nil ->
        {Nkey, Npid};
      {Pkey, _} ->
        %ligger den förslagna noden mellan mig och min pred
        case key:between(Nkey, Pkey, Id) of
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
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor)
  end.

create_probe(Id, Successor) ->
  %node ! probe
  {Skey, Spid} = Successor,
  Time = erlang:system_time(micro_seconds),
  Spid ! {probe, Id, [], Time},
  io:format("Sending away probe: {probe, ~w, ~w, ~w}~n", [Id, [], Time]).

remove_probe(T, Nodes) ->
  Time2 = erlang:system_time(micro_seconds),
  TimeFinal = Time2-T,
  io:format("Time: ~w ~n, Nodes: ~w", [TimeFinal, Nodes]).

forward_probe(Ref, T, Nodes, Id, Successor) ->
  {Skey, Spid} = Successor,
  List = lists:append([Id], Nodes),
  io:format("Forwarding probe: {probe, ~w, ~w, ~w}~n", [Ref, List, T]),
  Spid ! {probe, Ref, List, T}.


schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

stabilize({Skey, Spid}) ->
  Spid ! {request, self()}.

% frågar barnet vem dess pappa är, returnerar ALLTID en successor
% vill ha successor som retur argument, pga node status, pred
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

    % om barnet har en annan pappa än mig, som ligger mellan mig och barnet
    % föreslå mig själv som pappa till hans pappa
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        %det finns en annan nod emellan mig och barnet
        true ->
          Xpid ! {request, self()},
          Pred;
        false ->
          Spid ! {notify, {Id, self()}},
          Successor
        end
    end.
