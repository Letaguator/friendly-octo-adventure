% @author Mathias.Brekkan and ruiyang

-module(main).
-export([start/2, boss/4, sendAllRegAcc/6]).
-import(methods, [getRandomNumber/2, getRandomString/1, getHash/1, adjustToLinearBounds/2]).

start(NumberOfNodes, NumberOfRequests) ->
    Pid = spawn(main, boss, [NumberOfNodes, NumberOfRequests, [], {}]),
    register(master, Pid),
    createNodes(NumberOfNodes, node()).


createNodes(0, _) -> ok;
createNodes(NumberOfNodesLeft, MasterNode) ->
    spawn(main, nodeInit, [MasterNode]),
    createNodes(NumberOfNodesLeft - 1, MasterNode).

nodeInit(MasterNode) ->
    Id = getRandomString(8),
    Hid = getHash(Id),
    {master, MasterNode} ! {reg, self(), Hid},
    receive
        {allRegAcc, FingerList, NumberOfRequests} ->
            A = 1
    end.

buildFingerList(_, _, _, _, 0, FingerList) ->
    FingerList;
buildFingerList(CurrentIndex, NumberOfNodes, NodesSortedByHid, FingerTableSize, RemainingEntries, FingerList) ->
    NextNodeInListIndex = CurrentIndex + math:pow(FingerTableSize - RemainingEntries, 2),
    NextNodeInList = list:nth(NodesSortedByHid, adjustToLinearBounds(NextNodeInListIndex, NumberOfNodes)),
    buildFingerList(CurrentIndex, NumberOfNodes, NodesSortedByHid, FingerTableSize, RemainingEntries - 1, [NextNodeInList | FingerList]).

sendAllRegAcc(_, _, _, _, _, []) -> ok;
sendAllRegAcc(FingerTableSize, NumberOfNodes, CurrentIndex, NodesSortedByHid, NumberOfRequests, [Entry | Tail]) ->
    FingerList = buildFingerList(CurrentIndex + 1, NumberOfNodes, NodesSortedByHid, FingerTableSize, FingerTableSize, []),
    element(2, Entry) ! {allRegAcc, FingerList, NumberOfRequests},
    sendAllRegAcc(FingerTableSize, NumberOfNodes, CurrentIndex + 1, NodesSortedByHid, NumberOfRequests, Tail).

bossWaitForFinish(NumberOfNodesLeft) ->
    if
        NumberOfNodesLeft == 0 ->
            io:format("Finished running program...");
        true ->
            receive
                {finito} -> % Node completed all required requests
                    bossWaitForFinish(NumberOfNodesLeft - 1)
            end
    end.

boss(NumberOfNodes, NumberOfRequests, Nodes, NodesMap) ->
    case NumberOfNodes == length(Nodes) of
        true ->
            FingerTableSize = round(math:log2(NumberOfNodes)),
            NodesSortedByHid = lists:keysort(1, maps:to_list(NodesMap)),
            sendAllRegAcc(FingerTableSize, NumberOfNodes, 1, NodesSortedByHid, NumberOfRequests, NodesSortedByHid),
            bossWaitForFinish(NumberOfNodes);
            % Boss print average number of hops
        false ->
            ok
    end,
    receive
        {reg, Slave_ID, Hid} -> % Register node
            UpdatedNodesMap = maps:put(Hid, Slave_ID, NodesMap),
            boss(NumberOfNodes, NumberOfRequests, Nodes, UpdatedNodesMap)
    end.