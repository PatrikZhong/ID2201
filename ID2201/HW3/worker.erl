-module(worker).

-compile(export_all).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    rand:uniform(Seed),
    receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, time:zero());
		stop -> 
			ok
	end.

peers(Wrk, Peers) ->
  	Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Time2)->
    Wait = rand:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
			UpdatedTime = time:inc(Name,time:merge(Time2,Time)),
            Log ! {log, Name, UpdatedTime, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, UpdatedTime);
		stop -> ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    after 
		Wait ->
			Selected = select(Peers),
			Time = time:inc(Name,Time2),
            Message = {hello, rand:uniform(100)},
            Selected ! {msg, Time, Message},
			jitter(Jitter),
			Log ! {log, Name, Time, {sending, Message}},
			loop(Name, Log, Peers, Sleep, Jitter, Time)
	end.

select(Peers) ->
    lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) -> ok;

jitter(Jitter) -> timer:sleep(rand:uniform(Jitter)).


