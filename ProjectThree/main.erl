% @author Mathias.Brekkan and ruiyang

-module(main).
-export([start/2, boss/3, sendAllRegAcc/5]).

start(NumberOfNodes, NumberOfRequests) ->
    Pid = spawn(main, boss, [NumberOfNodes, NumberOfRequests]),
    register(master, Pid),
    createNodes(NumberOfNodes, node()).

getRandomNumber(Min, Max) ->
    crypto:rand_uniform(Min, Max + 1).

getHash(Value) ->
    crypto:sha1(Value).

createNodes(0, _) -> ok;
createNodes(NumberOfNodesLeft, MasterNode) ->
    spawn(main, nodeInit, [MasterNode]),
    createNodes(NumberOfNodesLeft - 1, MasterNode).

sendAllRegAcc(_, _, _, _, []) -> ok;
sendAllRegAcc(NumberOfNodes, CurrentIndex, Nodes, NumberOfRequests, [Node | Tail]) ->
    Node ! {allRegAcc, CurrentIndex, NumberOfRequests, Nodes},
    sendAllRegAcc(NumberOfNodes, CurrentIndex + 1, Nodes, NumberOfRequests, Tail).

boss(NumberOfNodes, NumberOfRequests, Nodes) ->
    case NumberOfNodes == length(Nodes) of
        true ->
            sendAllRegAcc(NumberOfNodes, 1, Nodes, NumberOfRequests, Nodes),
            StartTime = erlang:timestamp();
            % Boss wait to finish
            % Boss print average number of hops
        false ->
            ok
    end,
    receive
        {reg, Slave_ID} -> % Register node
            boss(NumberOfNodes, NumberOfRequests, [Slave_ID | Nodes])
    end.