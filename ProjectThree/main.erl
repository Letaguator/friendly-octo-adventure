% @author Mathias Brekkan and Ruiyang Li

-module(main).
-export([start/2, master/4, sendAllRegAcc/5, operate/5, nodeInit/2, join/1]).
-import(methods, [getRandomNumber/2, getRandomString/1, getHash/1, adjustToLinearBounds/2, getM/0]).
-import(test, [printList/1]).
-include("records.hrl"). 





start(NumberOfNodes, NumberOfRequests) ->
    %%% M = math:ceil(math:log2(NumberOfNodes * NumberOfRequests * 2)),
    Pid = spawn(main, master, [NumberOfNodes, NumberOfRequests, getM(), []]),
    register(master, Pid),
    createFirstNode(master).

master(NumberOfNodes, NumberOfRequests, M, Nodes) ->
    io:fwrite("Master\n"),
    % NodesSortedByHid = lists:keysort(1, maps:to_list(NodesMap)),
    
    %%% sendAllRegAcc(NumberOfNodes, 1, NumberOfRequests, Nodes, Nodes),
    %%% masterWaitForFinish(NumberOfNodes);
    % Boss print average number of hops
    receive
        {create, Node} -> % Register node
            io:format("Master reg node:\n"),
            io:format("~w~n", [Node]), 
            io:format("Current Nodes:~n"),
            UpdatedNodes = [Node | Nodes],
            printList(UpdatedNodes),
            Node#node.pid ! {create, NumberOfRequests},
            master(NumberOfNodes, NumberOfRequests, M, UpdatedNodes);
        {join, Node} ->
            io:format("Master: reg node:\n"),
            io:format("~w~n", [Node]), 
            io:format("Current Nodes:~n"),
            UpdatedNodes = [Node | Nodes],
            printList(UpdatedNodes),
            Node#node.pid ! {join, NumberOfRequests},
            master(NumberOfNodes, NumberOfRequests, M, UpdatedNodes)
            
    end.


createFirstNode(Master) ->
    io:format("Creating the first node~n"),
    spawn(main, nodeInit, [Master, true]).


join(Master) ->
    spawn(main, nodeInit, [Master, false]).



sendAllRegAcc(_, _, _, _, []) -> ok;
sendAllRegAcc(NumberOfNodes, CurrentIndex, NumberOfRequests, Nodes, [Entry | Tail]) ->
    if
        CurrentIndex == 1 ->
            Entry#node.pid ! {create, NumberOfRequests};
        true ->
            TargetNode = lists:nth(1, Nodes),
            Entry#node.pid ! {join, TargetNode, NumberOfRequests}
    end,
    sendAllRegAcc(NumberOfNodes, CurrentIndex + 1, NumberOfRequests, Nodes, Tail).

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

createNodes(0, _) -> ok;
createNodes(NumberOfNodesLeft, MasterNode) ->
    spawn(main, nodeInit, [MasterNode]),
    createNodes(NumberOfNodesLeft - 1, MasterNode).



nodeInit(MasterNode, IsFirstNode) ->
    RandomName = getRandomString(8),
    Node = #node{id = getHash(RandomName), pid = self(), key = RandomName},
    io:fwrite("Node init with ID: ~p~n", [Node#node.id]),
    case IsFirstNode of
        true ->
            master ! {create, Node};
        false ->        
            master ! {join, Node}
    end,
    
    receive
        {create, NumberOfRequests} ->
            Predecessor = nil,
            FingerList = lists:duplicate(getM(), Node),
            operate(MasterNode, NumberOfRequests, Node, Predecessor, FingerList);
        {join, KnownNode, NumberOfRequests} ->
            io:fwrite("Join\n"),
            io:write(self()),
            Predecessor = nil,
            KnownNode#node.pid ! {findSuccessor, Node#node.key, Node},
            receive
                {found, Key, FoundWhere, NumHops} ->
                    io:fwrite("Found successor\n"),
                    operate(MasterNode, NumberOfRequests, Node, Predecessor, [FoundWhere])
            end
    end.





operate(MasterNode, NumberOfRequestsLeft, Node, Predecessor, FingerList) ->
    io:format("Node is online:~n"),
    io:format("~w~n", [Node]),
    Successor = lists:nth(1, FingerList),
    NumHops = 1,
    receive
        {findSuccessor, Key, WhoAsked} -> 
            io:fwrite("Starting successor lookup\n"),
            findSuccessor(Key, Node, FingerList, Successor, WhoAsked, NumHops),
            operate(MasterNode, NumberOfRequestsLeft, Node, Predecessor, FingerList);
        {found, Key, FoundWhere, NumHops} ->
            io:format("Node: ~p~n", [self()]),
            io:format("Key: ~p~n", [Key#key.key]),
            io:format("Key identifier: ~p~n", [Key#key.id]),
            io:format("Found at node: ~p~n", [FoundWhere#node.pid]),
            io:format("Which as identifier: ~p~n", [FoundWhere#node.id]),
            io:format("Hops: ~p~n", [NumHops]),
            operate(MasterNode, NumberOfRequestsLeft - 1, Node, Predecessor, FingerList)
        % after 1000 ->
        %     if
        %         NumberOfRequestsLeft > 0 ->
        %             RandomKeyValue = getRandomString(8),
        %             HashedKey = getHash(RandomKeyValue),
        %             NewId = HashedKey rem round(math:pow(2, getM())),
        %             NewKey = #key{id = NewId, key = RandomKeyValue},
        %             findSuccessor(NewKey, Node, FingerList, Successor, Node, 0),
        %             operate(MasterNode, NumberOfRequestsLeft, Node, Predecessor, FingerList);
        %         true ->
        %             master ! {finito}
        %     end
    end.

stabilize(Node, Successor) ->
    % Get s
    ok.


notify() ->
    ok.

findSuccessor(Key, Node, FingerList, Successor, WhoAsked, NumHops) ->
    io:fwrite("Test\n"),
    io:write(Key#key.id),
    io:fwrite(" - "),
    io:write(Node#node.id),
    io:fwrite("\n"),
    if
        (Node#node.id == Successor#node.id) ->
            io:fwrite("Goal\n"),
            WhoAsked#node.pid ! {found, Key, Successor, NumHops};

        % TODO: What if successor is first node?
        (Key#key.id > Node#node.id) and (Key#key.id =< Successor#node.id) ->
            io:fwrite("Goal\n"),
            WhoAsked#node.pid ! {found, Key, Successor, NumHops};
        true ->
            io:fwrite("True case\n"),
            % TODO: replace 1 with something
            ClosestPrecedingNode = closestPrecedingNode(Key, Node, FingerList, 1, WhoAsked),
            ClosestPrecedingNode#node.pid ! {findSuccessor, Key, WhoAsked, NumHops + 1}
    end.

closestPrecedingNode(_, Node, _, 0, WhoAsked) ->
    Node;
closestPrecedingNode(Key, Node, FingerList, I, WhoAsked) ->
    FingerListElement = lists:nth(I, FingerList),
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