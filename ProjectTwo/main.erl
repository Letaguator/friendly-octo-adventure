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
createNodes(NumberOfNodesLeft, Master_Node) ->
    spawn(main, nodeInit, [Master_Node]),
    createNodes(NumberOfNodesLeft - 1, Master_Node).

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
            lists:nth(getRandomNumber(1, length(Nodes)), Nodes) ! {gossip, "Advanced message"},
            bossWaitForFinish(StartTime, NumberOfNodes);
        false ->
            ok
    end,
    receive
        {reg, Slave_ID} -> % Register node
            io:fwrite("Node registered\n"),
            boss(NumberOfNodes, GridType, AlgorithmType, [Slave_ID | Nodes])
    end.

bossWaitForFinish(StartTime, NumberOfNodesLeft) ->
    if
        NumberOfNodesLeft == 0 ->
            io:format("Finished, Program run time:~fs~n", [timer:now_diff(erlang:timestamp(), StartTime) / 1000000]);
        true ->
            receive
                {finito} ->
                    bossWaitForFinish(StartTime, NumberOfNodesLeft - 1)
            end
    end.

nodeInit(Master_Node) ->
    {master, Master_Node} ! {reg, self()},
    receive
        {allRegAcc, Index, GridType, AlgorithmType, Nodes} ->
            RandomNeighbour = getRandomNeighbour("FullNetwork", Index, Nodes),
            case AlgorithmType of
                "Gossip" ->
                    gossip(GridType, Master_Node, Index, Nodes, "", 0);
                "PushSum" ->
                    pushSum(GridType)
            end
    end.

pushSum(GridType) ->
    GridType,
    io:fwrite("pushSum").

gossip(GridType, Master_Node, Index, Nodes, ActualMessage, RecievedMessageCount) ->
    TerminationCount = 10,
    if
        RecievedMessageCount < TerminationCount ->
            receive
                {gossip, Message} ->
                    lists:nth(getRandomNeighbour(GridType, Index, Nodes), Nodes) ! {gossip, Message},
                    if
                        RecievedMessageCount == 1 ->
                            io:format("Message recieved first time:~p\n", [Message]),
                            {master, Master_Node} ! {finito};
                        true ->
                            ok
                    end,
                    gossip(GridType, Master_Node, Index, Nodes, Message, RecievedMessageCount + 1)
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

    