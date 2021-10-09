-module(map).
-compile(export_all).

new() -> %return empty list
    [].

update(Node, Links, Map) -> %delete node, append new Node
    
    DeletedMap = lists:keydelete(Node, 1, Map),
    [{Node, Links} | DeletedMap].

reachable(Node, Map) -> 
    case lists:keyfind(Node, 1, Map) of
        {_,Links} ->
            Links;
        false ->
            []
        end.



all_nodes(Map) -> 

    lists:foldl(fun listCreator/2, [], Map). 

listCreator({City, Neighbours}, Ackumulator) -> %tar första tupeln, gör tupeln till en lista, skickar den till isunique, 
%{Berlin, [Paris, London]} -> [Berlin, Paris, London] 
    lists:foldl(fun isUnique/2, Ackumulator, [City| Neighbours]).

isUnique(City, Ackumulator2) -> % i vår foldl, kolla varje city i [Berlin, Paris, London] om den är unik jämgört med ackumulatorn. Om unik, ta med ack.

    case lists:member(City, Ackumulator2) of 
        true -> 
            Ackumulator2;
        false -> 
            [City|Ackumulator2]
        end. 

    
