-module(vect).
-compile(export_all).

zero() -> [].

inc(Name, Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Tal} ->
      lists:keyreplace(Name, 1, Time, {Name, Tal+1});
    false ->
      [{Name, 1}|Time]
  end.

merge([], Time) ->
  Time;
merge([{Name, Ti}|Rest], Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Tj} ->
      [{Name, max(Ti, Tj)} | merge(Rest, lists:keydelete(Name, 1, Time))];
    false ->
      [{Name, Ti}|merge(Rest, Time)]
  end.


leq([], _) ->
  true;
leq([{Name, Ti}|Rest],Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Tj} ->
      if
        Ti =< Tj ->
          leq(Rest, Time); %kanske ta bort elementet i Time?
        true ->
          false
      end;
    false ->
      false
  end.

clock(_) ->
  [].

update(From, Time, Clock) ->
  {From, Received} = lists:keyfind(From, 1, Time),
    case lists:keyfind(From, 1, Clock) of
      {From, _} ->
        lists:keyreplace(From, 1, Clock, {From, Received});
      false ->
        [{From, Received} | Clock]
    end.

safe(Time, Clock) ->
  leq(Time, Clock).