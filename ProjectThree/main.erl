% @author Mathias.Brekkan and Ruiyang Li

-module(main).
-export([start/2, master/4, sendAllRegAcc/5, operate/5]).
-import(methods, [getRandomNumber/2, getRandomString/1, getHash/1, adjustToLinearBounds/2, getM/0]).
-include("records.hrl"). 

start(NumberOfNodes, NumberOfRequests) ->
    M = math:ceil(math:log2(NumberOfNodes * NumberOfRequests * 2)),
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
nodeInit(MasterNode) ->
    %%% in the paper the ip address is the key so I changed the name
    Node = #node{id = getHash(getRandomString(8)), pid = self()},
    {master, MasterNode} ! {reg, Node},

    %%% in erlang, receive, if, and case block export variables created in them
    %%% We just need to make sure all branches have the variable that will be called
    %%% outside of the block
    %%% if a variable is not called afterwards, we dont need to keep it safe

    receive
        {create, NumberOfRequests} ->
            Predecessor = nil,
            SuccessorNode = node(),
            operate(MasterNode, NumberOfRequests, Node, Predecessor, {});
        {join, Node, NumberOfRequests} ->
            Predecessor = nil,
            Node ! {findSuccessor, Node#node.id, node()},
            receive
                {foundSuccessor, SuccessorNode} ->
                    operate(MasterNode, NumberOfRequests, Node, Predecessor, {})
            end
    end.

operate(MasterNode, NumberOfRequestsLeft, Node, Predecessor, FingerList) ->
    % TODO:
    Successor = nil,
    NumHops = 5,
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
                    NewId = getHash(NewKey) rem math:pow(2, getM()),
                    NewKey = #key{id = NewId, key = NewKey},
                    findSuccessor(NewKey, Node, FingerList, Successor, Node, 0),
                    operate(MasterNode, NumberOfRequestsLeft - 1, Node, Predecessor, FingerList)
            end
    end.

sendAllRegAcc(_, _, _, _, []) -> ok;
sendAllRegAcc(NumberOfNodes, CurrentIndex, NumberOfRequests, Nodes, [Entry | Tail]) ->
    if
        CurrentIndex == 1 ->
            Entry ! {create, NumberOfRequests};
        true ->
            Entry ! {join, list:nth(Nodes, 1)}
    end,
    sendAllRegAcc(NumberOfNodes, CurrentIndex + 1, NumberOfRequests, Nodes, Tail).

findSuccessor(Key, Node, FingerList, Successor, WhoAsked, NumHops) ->
    if 
        (Key#key.id > Node#node.id) and (Key#key.id < Successor#node.id) ->
           WhoAsked#node.pid ! {found, Key, Successor, NumHops + 1};
        true ->
            % TODO: replace 1 with something
            ClosestPrecedingNode = closestPrecedingNode(Key, Node, FingerList, 1, WhoAsked),
            ClosestPrecedingNode#node.pid ! {Key, WhoAsked}
    end.

closestPrecedingNode(_, Node, _, 0, WhoAsked) ->
    Node;
closestPrecedingNode(Key, Node, FingerList, I, WhoAsked) ->
    FingerListElement = lists:nth(FingerList, I),
    if
        (FingerListElement#node.id > Node#node.id) and (FingerListElement#node.id < Key#key.id) ->
            NumHops = 1, % TODO: Fix
            WhoAsked#node.pid ! {found, Key, FingerListElement, NumHops};
        true ->
            closestPrecedingNode(Key, Node, FingerList, I - 1, WhoAsked)
    end.

buildFingerList(_, _, _, _, 0, FingerList) ->
    FingerList;
buildFingerList(CurrentIndex, NumberOfNodes, NodesSortedByHid, FingerTableSize, RemainingEntries, FingerList) ->
    NextNodeInListIndex = CurrentIndex + math:pow(2, FingerTableSize - RemainingEntries),
    NextNodeInList = list:nth(NodesSortedByHid, adjustToLinearBounds(NextNodeInListIndex, NumberOfNodes)),
    buildFingerList(CurrentIndex, NumberOfNodes, NodesSortedByHid, FingerTableSize, RemainingEntries - 1, [NextNodeInList | FingerList]).

master(NumberOfNodes, NumberOfRequests, Nodes, NodesMap) ->
    case NumberOfNodes == length(Nodes) of
        true ->
            % FingerTableSize = round(math:log2(NumberOfNodes)),
            % NodesSortedByHid = lists:keysort(1, maps:to_list(NodesMap)),
            sendAllRegAcc(NumberOfNodes, 1, NumberOfRequests, Nodes, Nodes),
            masterWaitForFinish(NumberOfNodes);
            % Boss print average number of hops
        false ->
            ok
    end,
    receive
        {reg, Slave_ID, Hid} -> % Register node
            UpdatedNodesMap = maps:put(Hid, Slave_ID, NodesMap),
            master(NumberOfNodes, NumberOfRequests, Nodes, UpdatedNodesMap)
    end.

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