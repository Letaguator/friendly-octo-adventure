% @author Mathias.Brekkan


-module(main).

-export([start/3, nodeInit/1, boss/2]).

% start(NumberOfNodes, fullNetwork, gossip) ->
%     io:fwrite("Test").

% start(NumberOfNodes, grid, gossip) ->
%     io:fwrite("Test").

% start(NumberOfNodes, line, gossip) ->
%     io:fwrite("Test").

% start(NumberOfNodes, imperfectGrid, gossip) ->
%     io:fwrite("Test").

% start(NumberOfNodes, fullNetwork, pushSum) ->
%     io:fwrite("Test").

% start(NumberOfNodes, grid, pushSum) ->
%     io:fwrite("Test").

% start(NumberOfNodes, line, pushSum) ->
%     io:fwrite("Test").

% start(NumberOfNodes, imperfectGrid, pushSum) ->
%     io:fwrite("Test").

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
            gossip(Master_Node, Index, Nodes)
    end.

gossip(Master_Node, Index, Nodes) ->
    receive
        {gossip, Message} ->                   
            io:fwrite("Message recieved:"),
            io:fwrite(Message),
            io:fwrite("\n"),
            gossip(Master_Node, Index, Nodes)   
    end.

% getRandomNeighbour(GridType, Index, Nodes) ->
%     1;
    
pushSum() ->
    io:fwrite("pushSum").