%%% Since in lerlang records only exist in the compiler
%%% We need to put macros and records into .hrl files and -include() in the main


%%% I suggest we should use records to keep track of keys and nodes
-record(node, {id, pid, key}). 
-record(key, {id, key}).