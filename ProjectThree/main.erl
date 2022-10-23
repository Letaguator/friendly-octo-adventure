% @author Mathias Brekkan and Ruiyang Li

-module(main).
-export([start/3, start/2, master/4, operate/6, nodeInit/2, join/1, createNodes/2]).
-import(methods, [getRandomNumber/2, getRandomString/1, getHash/1, adjustToLinearBounds/2, getM/0, getCircleSize/0]).
-import(test, [printList/1]).
-include("records.hrl"). 



start(NumberOfNodes, NumberOfRequests, Master) ->
    start(NumberOfNodes, NumberOfRequests),
    createNodes(NumberOfNodes, Master).


createNodes(0, _) ->
    io:format("All nodes are created~n");
createNodes(NumberOfNodes, Master) ->
    join(Master),
    createNodes(NumberOfNodes - 1, Master).



start(NumberOfNodes, NumberOfRequests) ->
    %%% M = math:ceil(math:log2(NumberOfNodes * NumberOfRequests * 2)),
    Pid = spawn(main, master, [NumberOfNodes, NumberOfRequests, getM(), []]),
    register(master, Pid),
    createFirstNode(master).

master(NumberOfNodes, NumberOfRequests, M, Nodes) ->
    % NodesSortedByHid = lists:keysort(1, maps:to_list(NodesMap)),
    
    %%% sendAllRegAcc(NumberOfNodes, 1, NumberOfRequests, Nodes, Nodes),
    %%% masterWaitForFinish(NumberOfNodes);
    % Boss print average number of hops
    receive
        {create, Node} -> % Register node
            io:format("Master initiate node:\n"),
            io:format("~w~n", [Node]), 
            io:format("Current Nodes:~n"),
            UpdatedNodes = [Node | Nodes],
            printList(UpdatedNodes),
            Node#node.pid ! {create, NumberOfRequests},
            master(NumberOfNodes, NumberOfRequests, M, UpdatedNodes);
        {join, Node} ->
            io:format("Master joining node: \n"),
            io:format("~w~n", [Node]), 
            io:format("Current Nodes:~n"),
            UpdatedNodes = [Node | Nodes],
            printList(UpdatedNodes),
            %%% find a random existing node in the network to initiate join
            Node#node.pid ! {join, lists:nth(getRandomNumber(1, length(Nodes)), Nodes), NumberOfRequests},
            master(NumberOfNodes, NumberOfRequests, M, UpdatedNodes)
    end.


createFirstNode(Master) ->
    io:format("Creating the first node~n"),
    spawn(main, nodeInit, [Master, true]).


join(Master) ->
    spawn(main, nodeInit, [Master, false]).


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


nodeInit(MasterNode, IsFirstNode) ->
    RandomName = getRandomString(8),
    Node = #node{id = getHash(RandomName), pid = self(), key = #key{key = RandomName, id = getHash(RandomName)}},
    case IsFirstNode of
        true ->
            master ! {create, Node};
        false ->        
            master ! {join, Node}
    end,
    
    receive

        {create, NumberOfRequests} ->
            Predecessor = Node,
            Successor = Node,
            FingerList = lists:duplicate(getM(), Node),
            io:format("Node is online:~n"),
            io:format("~w~n", [Node]),
            operate(MasterNode, NumberOfRequests, Node, Predecessor, Successor, FingerList);
        {join, KnownNode, NumberOfRequests} ->
            Predecessor = nil,
            KnownNode#node.pid ! {findSuccessor, Node#node.key, Node},
            receive
                {found, Key, FoundWhere, NumHops} ->
                    io:format("Node is online:~n"),
                    io:format("~w~n", [Node]),
                    %%% node_p needs to notify node_s that it needs to change predecessor
                    FoundWhere#node.pid ! {changePredecessor, Node},
                    operate(MasterNode, NumberOfRequests, Node, Predecessor, FoundWhere, [FoundWhere])
            end
    end.





operate(MasterNode, NumberOfRequestsLeft, Node, Predecessor, Successor, FingerList) ->
    receive
        {whatsYourPredecessor, WhoAsked} ->

            WhoAsked#node.pid ! {predecessor, Predecessor},

            operate(MasterNode, NumberOfRequestsLeft, Node, Predecessor, Successor, FingerList);
        {whatsYourSuccessor, WhoAsked} ->
            WhoAsked#node.pid ! {successor, Successor},
            operate(MasterNode, NumberOfRequestsLeft, Node, Predecessor, Successor, FingerList);
        {findSuccessor, Key, WhoAsked} -> 
            io:fwrite("Node:\n"),
            io:fwrite("~w~n", [self()]),
            io:fwrite("receieved findSuccessor request from:\n"),
            io:fwrite("~w~n", [WhoAsked]),
            NumHops = 420,
            findSuccessor(Key, Node, FingerList, Successor, WhoAsked, NumHops),
            operate(MasterNode, NumberOfRequestsLeft, Node, Predecessor, Successor, FingerList);



        {found, Key, FoundWhere, NumHops} ->
            io:format("Node: ~p~n", [self()]),
            io:format("Key: ~p~n", [Key#key.key]),
            io:format("Key identifier: ~p~n", [Key#key.id]),
            io:format("Found at node: ~p~n", [FoundWhere#node.pid]),
            io:format("Which as identifier: ~p~n", [FoundWhere#node.id]),
            io:format("Hops: ~p~n", [NumHops]),
            operate(MasterNode, NumberOfRequestsLeft - 1, Node, Predecessor, Successor, FingerList);

        {notify, NewPredecessor} ->
            io:format("Node:~n"),
            io:format("~w~n", [self()]),
            io:format("Is notified of:~n"),
            io:format("~w~n", [NewPredecessor]),
            
            if
                (Predecessor == nil) or ((NewPredecessor#node.id > Node#node.id) and (NewPredecessor#node.id < Predecessor#node.id)) ->
                    operate(MasterNode, NumberOfRequestsLeft - 1, Node, NewPredecessor, Successor, FingerList);
                true ->
                    operate(MasterNode, NumberOfRequestsLeft - 1, Node, Predecessor, Successor, FingerList)
            end;
        
        {changePredecessor, NewPredecessor} ->
            io:format("Node notified changePredecessor:~n"),
            io:format("~w~n", [Node]),
            io:format("to ~w~n~n", [NewPredecessor]),
            operate(MasterNode, NumberOfRequestsLeft, Node, NewPredecessor, Successor, FingerList)
        
        after 1000 ->
            io:format("Node run stablize:~n"),
            io:format("~w~n", [Node]),
            NewSuccessor = stabilize(Node, Successor, Predecessor),
            NewSuccessor#node.pid ! {notify, Node},
            operate(MasterNode, NumberOfRequestsLeft, Node, Predecessor, NewSuccessor, FingerList)
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



stabilize(Self, Successor, Predecessor) ->

    if
        Self == Successor ->
            Predecessor;
        true ->
            Successor#node.pid ! {whatsYourPredecessor, Self},
            CircleSize = getCircleSize(),
            receive
                {predecessor, SuccessorPecessor} ->
                    io:format("11111111111111111111"),
                    if
                        (Successor#node.id =< Self#node.id) and (SuccessorPecessor#node.id > Self#node.id) and (SuccessorPecessor#node.id < CircleSize) ->
                            io:format("New Node detected:~n"),
                            io:format("by ~w~n", [Self]),
                            io:format("of ~w~n", [SuccessorPecessor]),
                            SuccessorPecessor;
                        
                        (SuccessorPecessor#node.id > Self#node.id) and (SuccessorPecessor#node.id < Successor#node.id) ->
                            io:format("New Node detected:~n"),
                            io:format("by ~w~n", [Self]),
                            io:format("of ~w~n", [SuccessorPecessor]),
                            SuccessorPecessor;

                        true ->
                            Successor
                    end
                after 50 ->
                    io:format("Time out~n"),
                    Successor
            end
    end.



findSuccessor(Key, Node, FingerList, Successor, WhoAsked, NumHops) ->
    %%% io:fwrite("Test\n"),
    %%% io:write(Key#key.id),
    %%% io:fwrite(" - "),
    %%% io:write(Node#node.id),
    %%% io:fwrite("\n"),
    CircleSize = getCircleSize(),
    if
        (Node#node.id == Successor#node.id) ->
            io:fwrite("Goal case1\n"),
            WhoAsked#node.pid ! {found, Key, Successor, NumHops};
        
        

        % TODO: What if successor is first node?
        %%% one potential solution is to have a upper bound of the identifier value
        %%% all hash are modded by round(math:pow(2, getM()))
        %%% if Successor.id < Node.id and node.id < key.id < round(math:pow(2, getM()))
        %%% goal! 
        
        (Node#node.id > Successor#node.id) and ((Key#key.id > Node#node.id) and (Key#key.id =< CircleSize)) ->
            io:fwrite("Goal case2\n"),
            WhoAsked#node.pid ! {found, Key, Successor, NumHops};

        (Key#key.id > Node#node.id) and (Key#key.id =< Successor#node.id) ->
            io:fwrite("Goal case3\n"),
            %%% io:fwrite("Goal\n"),
            WhoAsked#node.pid ! {found, Key, Successor, NumHops};


        true ->
            %%% io:fwrite("True case\n"),
            % TODO: replace 1 with something
            ClosestPrecedingNode = closestPrecedingNode(Key, Node, FingerList, getM(), WhoAsked),
            ClosestPrecedingNode#node.pid ! {findSuccessor, Key, WhoAsked, NumHops + 1}
    end.

closestPrecedingNode(_, Node, _, 0, WhoAsked) ->
    Node;

%%% TODO: we need to change this so that all the finger list opperations can handle successor being 1
closestPrecedingNode(Key, Node, FingerList, I, WhoAsked) ->
    FingerListElement = lists:nth(I, FingerList),
    if
        (FingerListElement#node.id > Node#node.id) and (FingerListElement#node.id < Key#key.id) ->
            %%% NumHops = 1, % TODO: Fix
            %%% we only found the closest one not the final answer
            FingerListElement;
        true ->
            closestPrecedingNode(Key, Node, FingerList, I - 1, WhoAsked)
    end.




buildFingerList(_, _, _, _, 0, FingerList) ->
    FingerList;
buildFingerList(CurrentIndex, NumberOfNodes, NodesSortedByHid, FingerTableSize, RemainingEntries, FingerList) ->
    NextNodeInListIndex = CurrentIndex + math:pow(2, FingerTableSize - RemainingEntries),
    NextNodeInList = list:nth(NodesSortedByHid, adjustToLinearBounds(NextNodeInListIndex, NumberOfNodes)),
    buildFingerList(CurrentIndex, NumberOfNodes, NodesSortedByHid, FingerTableSize, RemainingEntries - 1, [NextNodeInList | FingerList]).