## Project 2

Mathias Brekkan
Ruiyang Li

### Compilation & Running
The following values can be tweaked in the code:
 - AmountOfWork
 - Target amount of coins
 - AmountOfWorkerNodes

Compiling code:
c(main.erl).

Running boss node with 5 worker nodes (Can be configured in code):
main:start(4). // params = NumberOfLeadingZeroesInHash


Start an amount of slaves NumberOfSlaves connecting to the boss specified in the next arg:
main:start_slaves(NumberOfSlaves, '<processName>@<nodeName>').

### What is working

### What is the largest network for each topology and algorithm
