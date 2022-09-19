%%% @author ruiyang
%%% run start_master/2 on the server
%%% run start_slave/1 on the working machine





-module(main).

-import(string, [len/1, equal/2, concat/2, chr/2, substr/3, str/2, to_lower/1, to_upper/1 ]).
-import(rnd, [rnd_chars_numbers/1]).
-import(binary, [decode_unsigned/1]).
-import(crypto, [hash/1]).

-export([start_master/2, start_slave/1, mine/3, slave/1, master/2]).









master(Work_load, N) ->
    receive
        {slave, Slave_ID} ->                                                
            Slave_ID ! {Work_load, N},
            master(Work_load, N);
        {found, Key, Hash} ->
            io:format("~p:", [Key]),
            io:format("~64.16.0b~n", [Hash]),
            master(Work_load, N);
        finished ->
            io:format("job done~n", []),
            master(Work_load, N)
    end.




mine(0, N, Master_Node) ->
    {master, Master_Node} ! finished;

mine(Work_load, N, Master_Node) ->
    Key = concat("liruiyang;", rnd:rnd_chars_numbers(10)),
    Hash = binary:decode_unsigned(crypto:hash(sha256, Key)),
    case Hash < math:pow(16, 64 - N) of
        true ->
            {master, Master_Node} ! {found, Key, Hash},
            mine(Work_load - 1, N, Master_Node);
        false ->
            mine(Work_load, N, Master_Node)
    end.




slave(Master_Node) ->
    {master, Master_Node} ! {slave, self()},
    
    receive
        {Work_load, N} ->
            mine(Work_load, N, Master_Node)
    end.





start_master(Work_load, N) ->
    register(master, spawn(main, master, [Work_load, N])).

start_slave(Master_Node) ->
    register(slave, spawn(main, slave, [Master_Node])).



