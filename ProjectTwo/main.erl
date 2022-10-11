% @author Mathias.Brekkan

-module(main).
-export([start/3, boss/4, nodeInit/1, sendAllRegAcc/5]).

% Todo: NumberOfNodes should be rounded to the nearest/easiest case where:
%       The following is a non-decimal number sqrt(NumberOfNodes) 
start(NumberOfNodes, GridType, AlgorithmType) ->
    Pid = spawn(main, boss, [NumberOfNodes, GridType, AlgorithmType, []]),
    register(master, Pid),
    createNodes(NumberOfNodes, node()).

createNodes(0, _) -> ok;
createNodes(NumberOfNodesLeft, MasterNode) ->
    spawn(main, nodeInit, [MasterNode]),
    createNodes(NumberOfNodesLeft - 1, MasterNode).

getRandomNumber(Min, Max) ->
    crypto:rand_uniform(Min, Max + 1).

getOneWithRandomSign() ->
    case getRandomNumber(0, 1) of
        0 ->
            1;
        1 ->
            -1
    end.

sendAllRegAcc(_, _, _, _, []) -> ok;
sendAllRegAcc(CurrentIndex, Nodes, GridType, AlgorithmType, [Node | Tail]) ->
    Node ! {allRegAcc, CurrentIndex, GridType, AlgorithmType, Nodes},
    sendAllRegAcc(CurrentIndex + 1, Nodes, GridType, AlgorithmType, Tail).

boss(NumberOfNodes, GridType, AlgorithmType, Nodes) ->
    case NumberOfNodes == length(Nodes) of
        true ->
            io:fwrite("Registered all nodes\n"),
            sendAllRegAcc(1, Nodes, GridType, AlgorithmType, Nodes),
            StartTime = erlang:timestamp(),
            if
                AlgorithmType == "Gossip" ->
                    lists:nth(getRandomNumber(1, length(Nodes)), Nodes) ! {gossip, "Advanced message"},
                    bossWaitForFinish(gossip, StartTime, NumberOfNodes);
                AlgorithmType == "PushSum" ->
                    lists:nth(getRandomNumber(1, length(Nodes)), Nodes) ! {pushSum, 0, 0},
                    bossWaitForFinish(pushSum, StartTime, Nodes)
            end;

            
        false ->
            ok
    end,
    receive
        {reg, Slave_ID} -> % Register node
            io:fwrite("Node registered\n"),
            boss(NumberOfNodes, GridType, AlgorithmType, [Slave_ID | Nodes])
    end.

bossWaitForFinish(gossip, StartTime, NumberOfNodesLeft) ->
    if
        NumberOfNodesLeft == 0 ->
            io:format("Finished, Program run time:~fs~n", [timer:now_diff(erlang:timestamp(), StartTime) / 1000000]);
        true ->
            receive
                {finito} ->
                    bossWaitForFinish(gossip, StartTime, NumberOfNodesLeft - 1)
            end
    end;


bossWaitForFinish(pushSum, StartTime, Nodes) ->
    receive
        {finito, Sum} ->
            io:format("The final sum is ~p~n", [Sum]),
            io:format("Program run time:~fs~n", [timer:now_diff(erlang:timestamp(), StartTime) / 1000000]),
            terminateAllNodes(Nodes)
    end.


terminateAllNodes([]) ->
    ok;
terminateAllNodes(Nodes) ->
    hd(Nodes) ! {kill},
    terminateAllNodes(tl(Nodes)).



nodeInit(MasterNode) ->
    {master, MasterNode} ! {reg, self()},
    receive
        {allRegAcc, Index, GridType, AlgorithmType, Nodes} ->
            RandomNeighbour = getRandomNeighbour("FullNetwork", Index, Nodes),
            case AlgorithmType of
                "Gossip" ->
                    gossip(GridType, MasterNode, Index, Nodes, "", 0);
                "PushSum" ->
                    pushSum(GridType, MasterNode, Index, Nodes, Index, 1, 0, [])
            end
    end.

pushSum(GridType, MasterNode, Index, Nodes, Sum, Weight, RecievedMessageCount, Ratios) ->
    receive
        {kill} ->
            ok;


        {pushSum, AddedSum, AddedWeight} ->
            NewSum = (Sum + AddedSum) / 2,
            NewWeight = (Weight + AddedWeight) / 2,



            case length(Ratios) of 
                3 ->
                    io:format("~p, ~p, ~p~n", Ratios),
                    IsFinished = (lists:nth(2, Ratios) - lists:nth(1, Ratios) < 0.0000000001) and (lists:nth(3, Ratios) - lists:nth(2, Ratios) < 0.0000000001),
                    if
                        IsFinished ->
                            NewRatios = [],
                            {master, MasterNode} ! {finito, NewSum / NewWeight};
                        
                        true ->
                           NewRatios = [tl(Ratios) | [NewSum / NewWeight]]
                    end;
                _Else ->
                    NewRatios = Ratios ++ [NewSum / NewWeight]
            end,



            io:format("Node ~p received ~p and ~p in the ~pth iteration~n", [self(), NewSum * 2, NewWeight * 2, RecievedMessageCount + 1]),
            lists:nth(getRandomNeighbour(GridType, Index, Nodes), Nodes) ! {pushSum, NewSum, NewWeight},
            pushSum(GridType, MasterNode, Index, Nodes, NewSum, NewWeight, RecievedMessageCount + 1, NewRatios)
    end.

            
    
gossip(GridType, MasterNode, Index, Nodes, ActualMessage, RecievedMessageCount) ->
    TerminationCount = 10,
    if
        RecievedMessageCount < TerminationCount ->
            receive
                {gossip, Message} ->
                    case RecievedMessageCount + 1 of
                        1 ->
                            io:format("Node ~p heard the ~pst gossip~n", [self(), RecievedMessageCount + 1]);
                        2 ->
                            io:format("Node ~p heard the ~pnd gossip~n", [self(), RecievedMessageCount + 1]);
                        _Else ->
                            io:format("Node ~p heard the ~pth gossip~n", [self(), RecievedMessageCount + 1])
                    end,
                    lists:nth(getRandomNeighbour(GridType, Index, Nodes), Nodes) ! {gossip, Message},
                    if
                        RecievedMessageCount == 1 ->
                            io:format("Message recieved first time:~p\n", [Message]),
                            {master, MasterNode} ! {finito};
                        true ->
                            ok
                    end,
                    gossip(GridType, MasterNode, Index, Nodes, Message, RecievedMessageCount + 1)
                after 50 ->
                    if
                        RecievedMessageCount > 0 ->
                            lists:nth(getRandomNeighbour(GridType, Index, Nodes), Nodes) ! {gossip, ActualMessage},
                            gossip(GridType, MasterNode, Index, Nodes, ActualMessage, RecievedMessageCount);
                        true ->
                            gossip(GridType, MasterNode, Index, Nodes, ActualMessage, RecievedMessageCount)
                    end
            end;
        true ->
            ok
    end.

adjustToLinearBounds(TargetIndex, Count) ->
    if
        TargetIndex > Count ->
            TargetIndex - Count;
        TargetIndex < 1 ->
            Count + TargetIndex;
        true ->
            TargetIndex
    end.

getRandomNeighbour(GridType, Index, Nodes) ->
    NodeCount = length(Nodes),
    case GridType of
        "FullNetwork" ->
            adjustToLinearBounds(Index + getRandomNumber(1, NodeCount), NodeCount);
        "Line" ->
            adjustToLinearBounds(Index + getOneWithRandomSign(), NodeCount);
        "2dGrid" ->
            GridWidth = round(math:sqrt(NodeCount)),
            getRandomGridNeighbour(GridWidth, Index);
        "Imperfect2dGrid" ->
            GridWidth = round(math:sqrt(NodeCount)),
            case getRandomNumber(1, 9) of
                1 ->
                    % Random neibour use
                    ok;
                2 ->
                    getRandomGridNeighbour(GridWidth, Index)
            end
    end.

getRandomGridNeighbour(GridWidth, Index) ->
    OffsetX = getRandomNumber(-1, 1),
    OffsetY = getRandomNumber(-1, 1),
    XCoord = Index rem GridWidth,
    YCoord = math:floor(Index/GridWidth),
    NewXCoord = adjustToLinearBounds(XCoord + OffsetX, GridWidth),
    NewYCoord = adjustToLinearBounds(YCoord + OffsetY, GridWidth),
    round(GridWidth * (NewXCoord - 1) + NewYCoord).


