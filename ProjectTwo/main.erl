% @author Mathias.Brekkan

-module(main).
-export([start/3, boss/4, nodeInit/1]).

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

sendAllRegAcc(CurrentIndex, Nodes, GridType, AlgorithmType, []) -> ok;
sendAllRegAcc(CurrentIndex, Nodes, GridType, AlgorithmType, [Node | Tail]) ->
    Node ! {allRegAcc, CurrentIndex, GridType, AlgorithmType, Nodes},
    sendAllRegAcc(CurrentIndex + 1, Nodes, GridType, AlgorithmType, Tail).

boss(NumberOfNodes, GridType, AlgorithmType, Nodes) ->
    case NumberOfNodes == length(Nodes) of
        true ->
            io:fwrite("\n"),
            io:write(lists:nth(getRandomNumber(1, length(Nodes)), Nodes)),
            io:fwrite("\n"),
            sendAllRegAcc(1, Nodes, GridType, AlgorithmType, Nodes),
            lists:nth(getRandomNumber(1, length(Nodes)), Nodes) ! {gossip, "Advanced message"};
        false ->
            ok
    end,
    receive
        {reg, Slave_ID} -> % Register node                            
            io:fwrite("Node registered\n"),
            boss(NumberOfNodes, GridType, AlgorithmType, [Slave_ID | Nodes])
    end.

nodeInit(Master_Node) ->
    {master, Master_Node} ! {reg, self()},
    receive
        {allRegAcc, Index, GridType, AlgorithmType, Nodes} ->
            case AlgorithmType of
                "Gossip" ->
                    gossip(GridType, Master_Node, Index, Nodes, "", 0);
                "PushSum" ->
                    pushSum(GridType)
            end
    end.

pushSum(GridType) ->
    io:fwrite("pushSum").

gossip(GridType, Master_Node, Index, Nodes, ActualMessage, RecievedMessageCount) ->
    TerminationCount = 10,
    if
        RecievedMessageCount < TerminationCount ->
            receive
                {gossip, Message} ->
                    io:format("Message recieved:~p\n", [Message]),
                    lists:nth(getRandomNeighbour(GridType, Index, Nodes), Nodes) ! {gossip, Message},
                    gossip(GridType, Master_Node, Index, Nodes, Message, RecievedMessageCount + 1)
            end;
        true ->
            lists:nth(getRandomNeighbour(GridType, Index, Nodes), Nodes) ! {gossip, ActualMessage},
            gossip(GridType, Master_Node, Index, Nodes, ActualMessage, RecievedMessageCount + 1)
    end.

adjustToLinearBounds(TargetIndex, Count) ->
    if
        TargetIndex > Count ->
            1;
        TargetIndex < 1 ->
            Count;
        true ->
            TargetIndex
    end.

getRandomNeighbour(GridType, Index, Nodes) ->
    NodeCount = length(Nodes),
    case GridType of
        "FullNetwork" ->
            % Currently node can select itself TODO!
            getRandomNumber(1, length(Nodes));
        "Line" ->
            case getRandomNumber(0, 1) of
                0 ->
                    adjustToLinearBounds(Index + 1, NodeCount);
                1 ->
                    adjustToLinearBounds(Index - 1, NodeCount)
            end;
        "2dGrid" -> 
            ok;
        "Imperfect2dGrid" ->
            ok
    end.
    