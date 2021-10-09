-module(intf).
-compile(export_all).

new() ->
  [].

add(Name, Ref, Pid, Intf) ->
  [{Name, Ref, Pid} | Intf].

remove(Name, Intf) ->
  lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    false ->
      notfound;
    {Name, _, Pid} ->
      {ok, Pid}
    end.

ref(Name, Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    false ->
      notfound;
    {Name, Ref, _} ->
      {ok, Ref}
    end.

name(Ref, Intf) ->
    case lists:keyfind(Ref, 2, Intf) of
      false ->
        notfound;
      {Name, Ref, _} ->
        {ok, Name}
      end.

list(Intf) ->
  lists:map(fun selector/1, Intf).

selector({Name, _, _}) ->
  Name.

broadcast(_, []) ->
empty;
broadcast(Message, [{_, _, Pid} | T]) ->
  Pid ! Message,

broadcast(Message, T).
