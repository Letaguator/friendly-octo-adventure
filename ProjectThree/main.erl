% @author Mathias.Brekkan and ruiyang

-module(main).
-export([start/2, master/4, sendAllRegAcc/6]).
-import(methods, [getRandomNumber/2, getRandomString/1, getHash/1, adjustToLinearBounds/2]).

start(NumberOfNodes, NumberOfRequests) ->
    M = math:ceil(log2(NumberOfNodes * NumberOfRequests * 2)),
    Pid = spawn(main, master, [M, NumberOfNodes, NumberOfRequests, [], {}]),
    register(master, Pid),
    createNodes(NumberOfNodes, node()).


createNodes(0, _) -> ok;
createNodes(NumberOfNodesLeft, MasterNode) ->
    spawn(main, nodeInit, [MasterNode]),
    createNodes(NumberOfNodesLeft - 1, MasterNode).

nodeInit(MasterNode) ->
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
            operate(FingerList, NumberOfRequests, Predecessor, Successor)
    end,
    



operate(FingerList, NumberOfRequestLeft) ->
    receive
        {lookUp, KeyId, WhoAsked} -> 

            if (KeyId) 
        
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
    sendAllRegAcc(FingerTableSize, NumberOfNodes, CurrentIndex + 1, NodesSortedByHid, NumberOfRequests, Tail).

masterWaitForFinish(NumberOfNodesLeft) ->
    if
        NumberOfNodesLeft == 0 ->
            io:format("Finished running program...");
        true ->
            receive
                {finito} -> % Node completed all required requests
                    masterWaitForFinish(NumberOfNodesLeft - 1)
            end
    end.

master(NumberOfNodes, NumberOfRequests, Nodes, NodesMap) ->
    case NumberOfNodes == length(Nodes) of
        true ->
            FingerTableSize = round(math:log2(NumberOfNodes)),
            NodesSortedByHid = lists:keysort(1, maps:to_list(NodesMap)),
            sendAllRegAcc(FingerTableSize, NumberOfNodes, 1, NodesSortedByHid, NumberOfRequests, NodesSortedByHid),
            masterWaitForFinish(NumberOfNodes);
            % master print average number of hops
        false ->
            ok
    end,
    receive
        {reg, Slave_ID, Hid} -> % Register node
            UpdatedNodesMap = maps:put(Hid, Slave_ID, NodesMap),
            master(NumberOfNodes, NumberOfRequests, Nodes, UpdatedNodesMap)
    end.