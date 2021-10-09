-module(node1Alex).
-export([start/1, start/2]).
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
  schedule_stabalize(),
  node(Id, Predecessor, Successor).

connect(Id, nil) ->
  {ok, {Id, self()}};
connect(_, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey, Peer}}
    after ?Timeout ->
      io:format("Time out: no response~n", [])
  end.

node(Id, Predecessor, Successor) ->
  receive
    {key, Qref, Peer} ->
      io:format("Node ~w: got key from ~w ~n", [Id, Peer]),
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
      node(Id, Predecessor, Successor);

    stop ->
      ok;

    Msg ->
      io:format("strange message: ~w~n", [Msg])
  end.

create_probe(Id, {_, Spid}) ->
  Time = erlang:system_time(micro_seconds),
  Spid ! {probe, Id, [], Time}.

remove_probe(T, Nodes) ->
  Time = erlang:system_time(micro_seconds) - T,
  io:format("Probe done, Time in micro_seconds: ~w~n Nodes: ~w~n", [Time, Nodes]).

forward_probe(Ref, T, Nodes, Id, {_, Spid}) ->
  Spid ! {probe, Ref, lists:append(Nodes, [Id]), T}.

stabilize({_, Spid}) ->
  Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    nil ->
      Spid ! {notify, {Id, self()}}, % we're before some pleb in queue
      Successor;

    {Id, _} ->
      Successor;

    {Skey, _} ->
      Spid ! {notify, {Id, self()}},
      Successor;

    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true ->

          Xpid ! {request, self()}, % before us in queue
		      Pred;

        false ->
          Spid ! {notify, {Id, self()}},
          Successor
      end
  end.

schedule_stabalize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};

    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.

notify({Nkey, Npid}, Id, Predecessor) ->
  case Predecessor of
    nil ->
      {Nkey, Npid};

    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          {Nkey, Npid};

        false ->
          Predecessor
      end
  end.










%
