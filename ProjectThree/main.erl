% @author Mathias.Brekkan and Ruiyang Li

-module(main).
-export([start/2, master/4, sendAllRegAcc/6]).
-import(methods, [getRandomNumber/2, getRandomString/1, getHash/1, adjustToLinearBounds/2, operate/5, getM/0]).
-include("records.hrl"). 


start(NumberOfNodes, NumberOfRequests) ->
    M = math:ceil(log2(NumberOfNodes * NumberOfRequests * 2)),
    Pid = spawn(main, master, [M, NumberOfNodes, NumberOfRequests, [], {}]),
    register(master, Pid),
    createNodes(NumberOfNodes, node()).

createNodes(0, _) -> ok;
createNodes(NumberOfNodesLeft, MasterNode) ->
    spawn(main, nodeInit, [MasterNode]),
    createNodes(NumberOfNodesLeft - 1, MasterNode).




%%% node API
%%% after creation, node process send itself out as a node record
%%% it then waits for the master to send back the finger list, the number of requests and the Successor as a node record 
node(MasterNode) ->
    %%% in the paper the ip address is the key so I changed the name
    Node = #node{id = getHash(getRandomString(8)), pid = self()},
    {master, MasterNode} ! {reg, Node},

    %%% in erlang, receive, if, and case block export variables created in them
    %%% We just need to make sure all branches have the variable that will be called
    %%% outside of the block
    %%% if a variable is not called afterwards, we dont need to keep it safe

    receive
        {allRegAcc, FingerList, NumberOfRequests, Successor} ->
            operate(Node, FingerList, NumberOfRequests, Successor)
    end.
    


findSuccessor(Key, Node, FingerList, Successor, WhoAsked, NumHops) ->
    if 
        (Key#key.id > Node#node.id) and (Key#key.id < Successor#node.id) ->
           WhoAsked#node.pid ! {found, Key, Successor, NumHops + 1};
        true ->
            ClosestPrecedingNode = closestPrecedingNode(Key, Node, FingerList, getM()),
            ClosestPrecedingNode#node.pid ! {Key, WhoAsked}
        end.


closestPrecedingNode(_, Node, _, 0, WhoAsked) ->
    Node;
closestPrecedingNode(Key, Node, FingerList, I, WhoAsked) ->
    if
        (List:nth(FingerList, I) > Node#node.id) and (List:nth(FingerList, I) < Key#key.id) ->
            WhoAsked#node.pid ! {found, Key, List:nth(FingerList, I), NumHops};
        true ->
            closestPrecedingNode(Key, Node, FingerList, I - 1)
    end.




operate(Node, FingerList, NumberOfRequestLeft, Successor) ->
    receive
        {findSuccessor, Key, WhoAsked} -> 

            findSuccessor(Key, Node, FingerList, Successor, WhoAsked, NumHops);

        {found, Key, FoundWhere, NumHops} ->
            io:format("Node: ~p~n", [self()]),
            io:format("Key: ~p~n", [Key#key.key]),
            io:format("Key identifier: ~p~n", [Key#key.id]),
            io:format("Found at node: ~p~n", [FoundWhere#node.pid]),
            io:format("Which as identifier: ~p~n", [FoundWhere#node.id]),
            io:format("Hops: ~p~n", [NumHops]),
            {master, MasterNode} ! {finito}
        after 1000 ->
            if
                NumberOfRequestsLeft > 0 ->
                    NewKey = getRandomString(8),
                    NewId = getHash(NewKey) rem math:pow(2, M),
                    NewKey = #key{id = NewId, key = NewKey},
                    findSuccessor(NewKey, Node, FingerList, Successor, Node, 0),
                    operate(NumberOfRequestLeft)
            end
    end.








buildFingerList(_, _, _, _, 0, FingerList) ->
    FingerList;
buildFingerList(CurrentIndex, NumberOfNodes, NodesSortedByHid, FingerTableSize, RemainingEntries, FingerList) ->
    NextNodeInListIndex = CurrentIndex + math:pow(2, FingerTableSize - RemainingEntries),
    NextNodeInList = list:nth(NodesSortedByHid, adjustToLinearBounds(NextNodeInListIndex, NumberOfNodes)),
    buildFingerList(CurrentIndex, NumberOfNodes, NodesSortedByHid, FingerTableSize, RemainingEntries - 1, [NextNodeInList | FingerList]).



sendAllRegAcc(_, _, _, _, _, []) -> ok;
sendAllRegAcc(FingerTableSize, NumberOfNodes, CurrentIndex, NodesSortedByHid, NumberOfRequests, [Entry | Tail]) ->
    FingerList = buildFingerList(CurrentIndex + 1, NumberOfNodes, NodesSortedByHid, FingerTableSize, FingerTableSize, []), = list:nth(NodesSortedByHid, adjustToLinearBounds(CurrentIndex - 1, NumberOfNodes)),
    Successor = list:nth(NodesSortedByHid, adjustToLinearBounds(CurrentIndex + 1, NumberOfNodes)),
    %%% PID of the node
    element(2, Entry) ! {allRegAcc, FingerList, NumberOfRequests, Successor},
        {create, NumberOfRequests} ->
     = nil,
            SuccessorNode = node(),
            nodeLoop(Hid, MasterNode, NumberOfRequests, {});
        {join, Node, NumberOfRequests} ->
     = nil,
            Node ! {findSuccessor, Hid, node()},
            receive
                {foundSuccessor, SuccessorNode} ->
                    nodeLoop(Hid, MasterNode, NumberOfRequests, {})
            end;
    end.

nodeLoop(Hid, MasterNode, NumberOfRequests, FingerTable) ->
    receive
        {findSuccessor, Hid, NodeAddress} ->
            if()
            ok;
        after -> 50:
            nodeLoop(Hid, MasterNode, NumberOfRequests)
    end.



% buildFingerList(_, _, _, _, 0, FingerList) ->
%     FingerList;
% buildFingerList(CurrentIndex, NumberOfNodes, NodesSortedByHid, FingerTableSize, RemainingEntries, FingerList) ->
%     NextNodeInListIndex = CurrentIndex + math:pow(FingerTableSize - RemainingEntries, 2),
%     NextNodeInList = list:nth(NodesSortedByHid, adjustToLinearBounds(NextNodeInListIndex, NumberOfNodes)),
%     buildFingerList(CurrentIndex, NumberOfNodes, NodesSortedByHid, FingerTableSize, RemainingEntries - 1, [NextNodeInList | FingerList]).

