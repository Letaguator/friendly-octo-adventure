## Project 3

Mathias Brekkan
Ruiyang Li

### Compilation & Running

Compiling code:
- c(methods).
- c(test).
- c(main). or c(main, [debug_info]).
- c(methods), c(test), c(main).

Running:
main:start(NumberOfNodes, NumberOfRequests, master).
join(Name of the current running node *when you use -sname).

### What is working
<<<<<<< Updated upstream
The code is close to completion and it does compile. There is however one or more bugs that we could not trace down in time, see the report for more details.
=======
The application is able to initiate the master node, which according to the user input,
initiate certain amout of node.
Each node periodically check for it's successor and finger table making sure the find successor functionality work properly.
Each node also send out one request for a random lookup per second. 
The code is close to completion, we just had some minor bugs blocking us from doing a run.
>>>>>>> Stashed changes

### What is the largest network we dealt with
We ran this on a network of 100 nodes with 2^16 of identifiers.

### Code snippets
Code snippets can be found in the report

*** After reading a Slack thread we thought our implementation matched what it was supposed to be. But to be safe we discussed with a TA, this discussion revealed that we had to do major changes in the architecture and that the Slack thread was incorrect. This cost us a lot of time and we ended up having some bugs that were very hard to trace down.