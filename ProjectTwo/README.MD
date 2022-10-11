## Project 2

Mathias Brekkan
Ruiyang Li

### Compilation & Running

Compiling code:
c(main).
    or
c(main, [debug_info]).

main:start(5, "FullNetwork", "Gossip").

First arg is number of nodes.

Options for network, arg 2:
 - FullNetwork
 - Line 
 - 2dGrid, for this argument the number of nodes must be return an integer if square root is taken
 - Imperfect3dGrid, for this argument the number of nodes must be dividable by 6 then return an integer if square root is taken of that value

Options for algorithm, arg 3:
 - Gossip
 - PushSum

Running boss node with 5 worker nodes (Can be configured in code):
main:start(4). // params = NumberOfLeadingZeroesInHash


Start an amount of slaves NumberOfSlaves connecting to the boss specified in the next arg:
main:start_slaves(NumberOfSlaves, '<processName>@<nodeName>').

### What is working
The code supports FullNetwork, Line, 2dGrid and (Imperfect3dGrid).
It can also utilize both the Gossip algorithm and the PushSum algorithm.

### What is the largest network for each topology and algorithm
Gossip:
 - FullNetwork: 10,000 nodes - Convergence after 0.333824s
 - Line: 10,000 nodes - Convergence after 7.440384s
 - 2dGrid: 10,000 nodes - Convergence after 0.259482s
 - Imperfect3dGrid

 PushSum:
 - FullNetwork
 - Line 
 - 2dGrid
 - Imperfect3dGrid

### Code snippets
Code snippets can be found in the report