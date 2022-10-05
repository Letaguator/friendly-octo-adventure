% @author Mathias.Brekkan


-module(main).

-export([start/3, nodeInit/1, boss/2]).

createNodes(0, _) -> ok;
createNodes(NumberOfNodesLeft, Master_Node) ->
    spawn(main, nodeInit, [Master_Node]),
    createNodes(NumberOfNodesLeft - 1, Master_Node).

start(NumberOfNodes, GridType, AlgorithmType) ->
    register(master, spawn(main, boss, [NumberOfNodes, []])),
    createNodes(NumberOfNodes, node()).

getRandomNumber(Min, Max) ->
    crypto:rand_uniform(Min, Max + 1).

sendAllRegAcc(CurrentIndex, Nodes, []) -> ok;
sendAllRegAcc(CurrentIndex, Nodes, [Node | Tail]) ->
    Node ! {allRegAcc, CurrentIndex, Nodes},
    sendAllRegAcc(CurrentIndex + 1, Nodes, Tail).

boss(NumberOfNodes, Nodes) ->
    case NumberOfNodes == length(Nodes) of
        true ->
            sendAllRegAcc(1, Nodes, Nodes),
            io:fwrite("\n"),
            io:write(lists:nth(getRandomNumber(1, length(Nodes)), Nodes)),
            io:fwrite("\n"),
            lists:nth(getRandomNumber(1, length(Nodes)), Nodes) ! {gossip, "Advanced message"};
        false ->
            ok
    end,
    receive
        {reg, Slave_ID} -> % Register node                            
            io:fwrite("Node registered\n"),
            boss(NumberOfNodes, [Slave_ID | Nodes])
    end.

nodeInit(Master_Node) ->
    {master, Master_Node} ! {reg, self()},
    receive
        {allRegAcc, Index, Nodes} ->
            gossip(Master_Node, Index, Nodes, "", 0)
    end.

gossip(Master_Node, Index, Nodes, ActualMessage, RecievedMessageCount) ->
    TerminationCount = 10,
    if
        RecievedMessageCount < TerminationCount ->
            receive
                {gossip, Message} ->
                    io:format("Message recieved:~p\n", [Message]),
                    lists:nth(getRandomNeighbour("FullNetwork", Index, Nodes), Nodes) ! {gossip, Message},
                    gossip(Master_Node, Index, Nodes, Message, RecievedMessageCount + 1)
            end;
        true ->
            lists:nth(getRandomNeighbour("FullNetwork", Index, Nodes), Nodes) ! {gossip, ActualMessage},
            gossip(Master_Node, Index, Nodes, ActualMessage, RecievedMessageCount + 1)
    end.

getRandomNeighbour(GridType, Index, Nodes) ->
    NodeCount = length(Nodes),
    case GridType of
        "FullNetwork" ->
            % Currently node can select itself TODO!
            getRandomNumber(1, length(Nodes));
        "Line" ->
            % Currently node can go outside range
            case getRandomNumber(0, 1) of
                0 ->
                    Target = Index + 1;
                1 ->
                    Target = Index - 1
            end;
        "2dGrid" -> 
            ok;
        "Imperfect2dGrid" ->
            ok
    end.
    
pushSum() ->
    io:fwrite("pushSum").