-module(hist).
-compile(export_all).


new(Name) ->
  [{Name, inf}].

update(Node, N, History) ->
  T = lists:keyfind(Node, 1, History),
  case T of
    false -> {new, [{Node, N}|History]};
    T -> {_, Old} = T,
    if Old < N ->
      {new, lists:keyreplace(Node, 1, History, {Node, N})};
    true->
      old
    end
  end.

                
