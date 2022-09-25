## Project 1

Ruiyang Li
Mathias Brekkan

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

### Optimal Size of work
We tested different workload for each worker by giving each worker different number of work at a single time.
we ran experiment for each combination 50 times.
number_of_node  work_per_node   ave_total_runtime
10   1 0.077483   
10   2  0.071621
10   5  0.060934
10  10  0.071773
As we can see from the above statistics. The optimal number of workload per node per run should be around 5.


### Result for input 4
(master@Laptop)2> main:start(4). 
CPU Time - Time Passed Ratio: true
0.1312
(master@Laptop)3> CPU Time - Time Passed Ratio: 4.8718
CPU Time - Time Passed Ratio: 4.95
"liruiyang;is8x1in5i1":0000cb3c6bdfac1208377bb7097200d86c5bc8c75034b2bc8eba9cdf73a210f6
"liruiyang;j82lmvv3st":000047be547f9a5aee591427c656d3931dd2d5e2b8925b4aabba8c75fe6847ac
"liruiyang;bf2czzlky5":0000e2d5598c60d85df1e11baa7fb898cf9bbd7b6c3be62cb895061a4fb793c1
"liruiyang;rw69jwa1u6":0000eef8f339ee260790c2a079d636a97495b69cb5b3a322f512ed73bdd1f91f
"liruiyang;5ntenswnml":0000923938604e63808f0fa856174aa12347595cb1d8e74a2729fc4cbcca61b6
"liruiyang;gsltrnhw19":00008c88ae6330469878d828f7657b8d2e9cdd7274d7d938819009cf85ac2b53
job done                 
"liruiyang;splo1owk5a":00003d6c2df68e52024fb0873178ffc861fe7a89312b6a31310dbaa57129519c
"liruiyang;jqoiou5w2z":0000e4e383a8ae5d7ee37acf7588908ec5038124d9670239c0c4c23fe681773e
"liruiyang;aax1f1czmf":000039de849759610963cc17e0293aa237669bd3a60654ae1b1f1489005c1f2c
"liruiyang;k98w2w4j6g":000053ab8f31d8f9a000f51d999c58dbc351caed2afbeafd9323b203ca8c2a71
"liruiyang;v2l8hw1key":000082b91a5a866c8e7fce573437222e51dd54ffbb4d70ce65148b3e2ca158d2
"liruiyang;j2gi817gcu":0000ea94130ae3e017321562ca944b21fb2e920fe8289b6bb31eac044db4e8fb
job done                 
(master@Laptop)3> CPU Time - Time Passed Ratio: 4.922
"liruiyang;owt88akezp":00004161253825f96e0bba19cda03f449e92ab34a4ebc083fc072a20f7f63224
"liruiyang;h3umfov7h7":0000782fc884972f140942ca176f36587a0bc5fdeb33a939f041aee296317986
"liruiyang;jrte9qjr1a":00004af454b64683378a5aa2643ea1ba9ab5263dbd5fedb8290f48c857b591eb
"liruiyang;uefj7xq7jk":0000732a05711919dd16a5d0fcab03fbd40b025063c7ead4243b11e200eae424
"liruiyang;7bw1bpj9vo":0000adbce5448842ff92eb2bb13ce4b918254f660eb98a3418d2775af3e602d1
"liruiyang;2hhdi64h2q":0000e286c0bf93cb6132651f085257cb11242e067dca35847c9c1a69a50e9afc
job done                 
(master@Laptop)3> CPU Time - Time Passed Ratio: 4.8968
"liruiyang;smr8gjilzd":000036b4b9d0749cd2005ab7c09be45f7be055658cb86d1bd911f5df9e9a72ae
"liruiyang;qc3drgson3":0000984fa439a6381f43ad4db3ea2a4db09078817c30c9bcb7e03f439d2378c3
"liruiyang;hqxu1l185o":0000bca33bb9fd670139a297e5e471749949cfd1d3a93c9d93b0858f4b44d85c
"liruiyang;d9ac5jvut4":0000d939d9d080c00ea70fc13d9bfc00c4650f82210ccbc61855fb1797272be4
"liruiyang;18x26fy7h4":00004dd5b5a5208817bdae864830b3b70888419fe3c541c360a853ee07e5486d
"liruiyang;v33167v642":0000dc6a34870557dac1c563b72824ebae1d3ef797652fb0dbba542ceb424722
job done                 
(master@Laptop)3> CPU Time - Time Passed Ratio: 4.9062
"liruiyang;mukq9qaiqy":00008d021b67fc710f8aeaa5baac93471040d14debaba858d8fc639eb9e8bbee
"liruiyang;woo6wbc5e1":00009711e544f7314fd841a95b70be9efde87c22788b71555569c45e0bb53a62
"liruiyang;8obyx1unxx":0000e53020bca2bfb9ba0f9348f24fbcfeacbbbc346dac559ec44218d34f6d98
"liruiyang;67mfz5dx9v":000097e20aa7704973db4f837721d40b70d0324d430e739bf7da0d0b5cc154d2
"liruiyang;efg1woiq8i":0000e4ad7af2f61646550705fc329b9fffe0b6fb70c477e41e045697bade3b2c
"liruiyang;amgtt89oue":0000d944820946fc7d2280b477637c5fe1a3cbfe2aa1bab90200be01e4e25edc
job done                 
"liruiyang;9ebnhu1fxf":0000b3df21dee83456d2c3dfec68bf54dadc15f656ae208528bbe87b7b0f6ee2
"liruiyang;exqmlmgonz":0000b475471570fbcdf352396d37ec6bca6b15a1cfe4842fd9ce83787dffcac6
"liruiyang;82dxtk2qwj":0000eeb9feb5cbb8f2086b928ecb7964a7a724caa188447e325b113bcb21142d
"liruiyang;69411aclym":0000c1042cbef7b51586e80f3013ae88ac5b07283416eee4f6f9d57526692152
"liruiyang;vtkjbx5t2v":0000855a3b196ed945d222e1eaed5a9c67f3725582671b97eea6c11b9c256584
"liruiyang;ycfy345mpa":000069a49d0bfe6c26d9c4bc030e6217fa85d2d62feb0ebfdd6a352092b9057f
job done                 
(master@Laptop)3> CPU Time - Time Passed Ratio: 4.9126
"liruiyang;e2zv7dp7is":0000d2355727fba3ae7f544b7b57ad93b766090128c092474464ca5fe8c7cccd
"liruiyang;584o16ptb3":00004d2f1698ef7df6d5b5c74b98ba8495cd5901b7c82aa51e9374d1f2db02d5
"liruiyang;a71dcaqsmw":0000f87bcdc3270367e846e6543c1f10b1daa10a4f59dd0219f7efd9e7c4f3cf
"liruiyang;s7btr37tdo":0000df6cad42bf0a2564bd70908670c6a418d8ef09637c2c556b4b28ee7a0309
"liruiyang;ppv32pcxdm":00009798b8d1d9009aef8aadc69adbd3e85bdb83928f2acc4210c4a4f3b1f0b2
"liruiyang;sowfaj89y2":0000a01ff88374d82250028c68293f0298add03d5e8a0f00df749d4586a08faf
job done                 
Program run time:30.754304s

### Ratio of CPU time to Real time
The way we calculate this ratio is by every 5 seconds doing a measure by checking how much CPU time the Erlang VM used the previous 5 seconds. We then devide this value by the 5 seconds in order to get our actual ratio. The weakness with this strategy is that we have to not run other Erlang programs and the VM itself can influence the result.

When running with the boss actor, performance actor and 12 worker nodes the resulting ratio was usually in the 7.5->7.95. An example value we read was: 11.34. This was on a computer with 6 cores, 12 threads.

### Coin with most 0s found (7 leading 0s)
"liruiyang;hs1y8d9tuc" : 0000000b6a7e46d1e4e78f828c5b8a11bb504a4ccea40091126eb2094fafa728

### Largest number of working machines ran on
Due to the lack of LAN and good ways to connect our PCs, we decided to run it on a single computer. That did not stop us from running with up to 12 worker actors, on a 6 core computer with a total of 12 threads. We did this with one node, but also with more nodes on the same PC. The expermient which we tried and documented the most was on Mathias computer, on which we ran 8 worker actors. by setting AmountOfWorkerNodes = 8. This led to a ratio of CPU time to real time around 7.70-7.92. This is an very good amount of parallelism!