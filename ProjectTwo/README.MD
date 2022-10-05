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
 - 2dGrid
 - Imperfect2dGrid

Options for algorithm, arg 3:
 - Gossip
 - PushSum

Running boss node with 5 worker nodes (Can be configured in code):
main:start(4). // params = NumberOfLeadingZeroesInHash


Start an amount of slaves NumberOfSlaves connecting to the boss specified in the next arg:
main:start_slaves(NumberOfSlaves, '<processName>@<nodeName>').

### What is working

### What is the largest network for each topology and algorithm
