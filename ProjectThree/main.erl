% @author Mathias.Brekkan and ruiyang

-module(main).
-export([start/2, master/4, sendAllRegAcc/6]).
-import(methods, [getRandomNumber/2, getRandomString/1, getHash/1, adjustToLinearBounds/2, operate/5, getM/0]).

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
%%% after creation, node process send out its PID and identifier {reg, PID, identifier}
%%% waits for the master to send back the finger list, the number of requests the PID and the identifier of the predecessor and the Successor
node(MasterNode) ->
    %%% in the paper the ip address is the key so I changed the name
    NodeKey = getRandomString(8),
    NodeId = getHash(NodeKey),
    {master, MasterNode} ! {reg, self(), NodeId},

    %%% in erlang, receive, if, and case block export variables created in them
    %%% We just need to make sure all branches have the variable that will be called
    %%% outside of the block
    %%% if a variable is not called afterwards, we dont need to keep it safe

    receive
        {allRegAcc, FingerList, NumberOfRequests, Predecessor, Successor} ->
            operate(NodeId, FingerList, NumberOfRequests, Predecessor, Successor)
    end,
    


findSuccessor(NodeId, FingerList, Predecessor, Successor) ->
    if 
        ((KeyId > NodeId) and (KeyId < Successor#node.id) ->
            Successor;
        true ->
            closestPrecedingNode(KeyId).

closestPrecedingNode(KeyId, FingerList, ) ->





operate(NodeId, FingerList, NumberOfRequestLeft, Predecessor, Successor) ->
    receive
        {findSuccessor, KeyId, WhoAsked} -> 

            Res = findSuccessor(NodeId, FingerList, Predecessor, Successor),
            WhoAsked ! {KeyId, Successor, NumHops}

        {found, KeyId, FoundWhere, NumHops} ->
            io:format("Node: ~p~n", [self()]),
            io:format("Key: ~p~n", [KeyId]),
            io:format("Hash: ~p~n" [Hash]),
            io:format("Found at:~p~n", [FoundWhere]),
            io:format("Hops: ~p~n", [NumHops]),
            {master, MasterNode} ! {finito}
        after 1000 ->
            if
                NumberOfRequestsLeft > 0 ->
                    NewId = getHash(getRandomString(8)) rem math:pow(2, M),
                    lookUp(NewId, self()),
                    operate(NumberOfRequestLeft)
            end
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
    FingerList = buildFingerList(CurrentIndex + 1, NumberOfNodes, NodesSortedByHid, FingerTableSize, FingerTableSize, []),
    Predecessor = list:nth(NodesSortedByHid, adjustToLinearBounds(CurrentIndex - 1, NumberOfNodes)),
    Successor = list:nth(NodesSortedByHid, adjustToLinearBounds(CurrentIndex + 1, NumberOfNodes)),
    %%% PID of the node
    element(2, Entry) ! {allRegAcc, FingerList, NumberOfRequests, Predecessor, Successor},
        {create, NumberOfRequests} ->
            Predecessor = nil,
            SuccessorNode = node(),
            nodeLoop(Hid, MasterNode, NumberOfRequests, Predecessor, {});
        {join, Node, NumberOfRequests} ->
            Predecessor = nil,
            Node ! {findSuccessor, Hid, node()},
            receive
                {foundSuccessor, SuccessorNode} ->
                    nodeLoop(Hid, MasterNode, NumberOfRequests, Predecessor, {})
            end;
    end.

nodeLoop(Hid, MasterNode, NumberOfRequests, Predecessor, FingerTable) ->
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

