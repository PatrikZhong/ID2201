-module(dijkstra).
-compile(export_all).

%Skapat med hjälp av Klas Segeljakt från hans föreläsning, 2021-09-21.

entry(Node, Sorted) ->
  case lists:keyfind(Node, 1, Sorted) of
    {Node, Length, _} ->
      Length;
    false ->
      0
    end.

replace(Node, N, Gateway, Sorted) ->
  Result = lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway}),
  F = fun({_, Old, _}, {_, New, _}) -> Old < New end,
  lists:sort(F, Result).

update(Node, N, Gateway, Sorted) ->
  Old_N = entry(Node, Sorted),
  if
    N < Old_N ->
      replace(Node, N, Gateway, Sorted);
    true ->
      Sorted
    end.

  iterate(Sorted, Map) ->
    Table = [],
    iterate(Sorted, Map, Table).

  iterate([], _, Table) ->
    Table;

  iterate([{_, inf, _}| _], _, Table) ->
    Table;

  iterate([{Node, N, Gateway} | Sorted], Map, Table) ->
    Links = map:reachable(Node, Map),
    F = fun(Link, Sorted1) ->
      update(Link, N+1, Gateway, Sorted1)
    end,
    UpdatedSorted = lists:foldl(F, Sorted, Links),
    UpdatedTable = [{Node, Gateway} | Table],
    iterate(UpdatedSorted, Map, UpdatedTable).

  table(Gateways, Map) ->
    Nodes = map:all_nodes(Map),
    Union = lists:usort(Nodes ++ Gateways),
    Node_F = fun(Node) ->
      {Node, inf, unknown}
    end,
    SortedList1 = lists:map(Node_F, Union),
    Gateway_F = fun(Gateway, Sorted) ->
      update(Gateway, 1, Gateway, Sorted)
    end,
    SortedList2 = lists:foldl(Gateway_F, SortedList1, Gateways),
    iterate(SortedList2, Map).

  route(Node, Table) ->
    case lists:keyfind(Node, 1, Table) of
      {_, Gateway} ->
        {ok, Gateway};
      false ->
        notfound
      end.
