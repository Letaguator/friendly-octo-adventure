## Project 1

Ruiyang Li
Mathias Brekkan

### Compilation & Running
Compiling code:
c(main.erl).

Running boss node with 5 worker nodes (Can be configured in code):
main:start(10, 1). // params = AmountOfCoins, LeadingZerosForCoin


Start an amount of slaves NumberOfSlaves connecting to the boss specified in the next arg:
main:start_slaves(NumberOfSlaves, '<processName>@<nodeName>').

### Optimal Size of work

### Result for input 4
"liruiyang;7mr85bqyzm" : 0000ad6ebfd97b4d6d797cba0e5e4e2e7ddef47eff56b2043422394c78b1bffd
"liruiyang;qaxe6r71ps" : 00001bf65292c44f0968191ca17f2c67c196b697abcff9318db4b11e51fdc416
"liruiyang;mpx91tgfhq" : 000080e61197c4fb1de22e91fbddd37b84fab10eab88a3315cb189ef20b17d7e
"liruiyang;et7587vvw4" : 0000112200897238c9e7739fd9a19fa7ee65db2bbbe9fd8b0533c076d8eb00e8
"liruiyang;zqvy43lix5" : 0000358b1a0de769544c903c80bc04acb32452b826a0dca8615f8b64a4886e7b
"liruiyang;k95sdt8blf" : 0000fdd1096bbce6cf01642e5a61c920030a93417ed4f82b5e82f9179c49e9af
(master@Laptop-Waldur)3> CPU Time - Time Passed Ratio: 4.9064
"liruiyang;kuqk746s1s" : 00008f42bacf815be422e929d02d7bca67858629760f48d6a6f4c6c7f19f7ad7
"liruiyang;nalmocvzpm" : 0000e01222a6769511092486a5dcc8d9541872611bcadaaf3fd128edd6bfd00c
"liruiyang;zlrh5y6blj" : 0000523e3a44b89501bc0fb5cdc5fd37a1baf1ec3cf660dec1440d1475bb5521
"liruiyang;zgzuf2mud9" : 0000bc80c69658073f8b864cf4359f4808815a4d2c9360df684d4b02dcfbaed6
"liruiyang;h3i5z3ztf5" : 00006011c060e07276a40f87b6a1ab22a8d2e2e376916815cdc6dd5757f0cd49


### Ratio of CPU time to Real time
The way we calculate this ratio is by every 5 seconds doing a measure by checking how much CPU time the Erlang VM used the previous 5 seconds. We then devide this value by the 5 seconds in order to get our actual ratio. The weakness with this strategy is that we have to not run other Erlang programs and the VM itself can influence the result.

When running with the boss actor, performance actor and 12 worker nodes the resulting ratio was usually in the 7.5->7.95. An example value we read was: 11.34. This was on a computer with 6 cores, 12 threads.

### Coin with most 0s found (7 leading 0s)
"liruiyang;hs1y8d9tuc" : 0000000b6a7e46d1e4e78f828c5b8a11bb504a4ccea40091126eb2094fafa728

### Largest number of working machines ran on
Due to the lack of LAN and good ways to connect our PCs, we decided to run it on a single computer. That did not stop us from running with up to 12 worker actors, on a 6 core computer with a total of 12 threads. We did this with one node, but also with more nodes on the same PC. The expermient which we tried and documented the most was on Mathias computer, on which we ran 8 worker actors. by setting AmountOfWorkerNodes = 8. This led to a ratio of CPU time to real time around 7.70-7.92. This is an very good amount of parallelism!