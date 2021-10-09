-module(loggy).

-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
   	Logger ! stop.

init(Nodes) ->
	loop(time:clock(Nodes), []).

loop(Clock, HoldBackQueue) ->
	receive
		{log, From, Time, Msg} ->
			NewClock = time:update(From, Time, Clock),
			
			UpdatedQueue = [{From, Time, Msg}|HoldBackQueue],
			
			SortedHoldBackQueue = lists:keysort(2, UpdatedQueue),
			
			NewHBQ = iterateSafeList(SortedHoldBackQueue, NewClock, []),
			
			loop(NewClock, NewHBQ);
		stop ->
			
			ok
	end.

log(From, Time, Msg) ->
	io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

iterateSafeList([], _, AppendList)-> %ta emot en sorterad holdbackqueue, ta emot NewClock, 
	AppendList;
iterateSafeList([{From, Time, Msg}|T], Clock, Acc)->
	case time:safe(Time, Clock) of
		true ->
			log(From, Time, Msg),
			iterateSafeList(T, Clock, Acc);
		false->			
			iterateSafeList(T, Clock, [{From, Time, Msg}|Acc])
	end.