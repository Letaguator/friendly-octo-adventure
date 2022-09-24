%%% @author ruiyang & mathias.brekkan
%%% run start_master/2 on the server
%%% run start_slave/1 on the working machine

% N is the number of leading zeros for a potential coin
%%% t0 change the amount of threads go to start_master
%%% to change the number of coins mined go to  start

-module(main).

-import(string, [len/1, equal/2, concat/2, chr/2, substr/3, str/2, to_lower/1, to_upper/1 ]).
-import(rnd, [rnd_chars_numbers/1]).
-import(binary, [decode_unsigned/1]).
-import(crypto, [hash/1]).
-import(timer, [apply_after/4, now_diff/2]).

-export([start/1, start_master/2, mine/4, slave/1, master/6, start_slaves/2, start_perf_analyzer/2]).


get_random_string(Length) ->
  AllowedChars = "abcdefghijklmnopqrstuvwxyz1234567890",
  MaxLength = length(AllowedChars),
  lists:foldl(
    fun(_, Acc) -> [lists:nth(crypto:rand_uniform(1, MaxLength), AllowedChars)] ++ Acc end,
    [], lists:seq(1, Length)
).

master(main, WorkerNodeCount, TargetAmountOfCoins, N, CoinMined, StartTime) ->
    start_perf_analyzer(0, self()),
    start_slaves(WorkerNodeCount, node()),
    master(sub, WorkerNodeCount,  TargetAmountOfCoins, N, CoinMined, StartTime);

master(sub, WorkerNodeCount, TargetAmountOfCoins , N, CoinMined, StartTime) ->
    AmountOfWork = 6,
    if
        TargetAmountOfCoins =< CoinMined ->
            io:format("Program run time:~fs~n", [now_diff(erlang:timestamp(), StartTime) / 1000000]),
            exit(done);
        true -> ok
    end,
    receive
        {slave, Slave_ID} ->                                                
            Slave_ID ! {AmountOfWork, N},
            master(sub, WorkerNodeCount, TargetAmountOfCoins, N, CoinMined, StartTime);
        {finished, CoinsFound} ->
            lists:foreach(fun(Entry) -> printEntry(Entry) end, CoinsFound),
            io:format("job done~n", []),
            master(sub, WorkerNodeCount, TargetAmountOfCoins, N, CoinMined + length(CoinsFound), StartTime)
    end.

printEntry({Key, Hash}) ->
    io:format("~p:", [Key]),
    io:format("~64.16.0b~n", [Hash]).

mine(0, _, Master_Node, CoinsFound) ->
    {master, Master_Node} ! {finished, CoinsFound};

mine(AmountOfCoinsLeft, N, Master_Node, CoinsFound) ->
    Key = concat("liruiyang;", get_random_string(10)),
    Hash = binary:decode_unsigned(crypto:hash(sha256, Key)),
    case Hash < math:pow(16, 64 - N) of
        true ->
            mine(AmountOfCoinsLeft - 1, N, Master_Node, [{Key, Hash} | CoinsFound]);
        false ->
            mine(AmountOfCoinsLeft, N, Master_Node, CoinsFound)
    end.

slave(Master_Node) ->
    {master, Master_Node} ! {slave, self()},
    
    receive
        {AmountOfCoins, N} ->
            mine(AmountOfCoins, N, Master_Node, [])
    end,
    slave(Master_Node).


start_slaves(0, _) -> ok;
start_slaves(N, Master_Node) ->
    spawn(main, slave, [Master_Node]),
    start_slaves(N - 1, Master_Node).

start(NumberOfLeadingZeroesInHash) ->
    %%% Here to change the amount of coin mined by the entire program
    start_master(40, NumberOfLeadingZeroesInHash).

start_perf_analyzer(LastCpuTime, Master_PID) ->
    case is_process_alive(Master_PID) of
        false ->
            ok;
        true ->
            {CpuTime, _} = statistics(runtime),
            io:fwrite("CPU Time - Time Passed Ratio: "),
            io:write((CpuTime - LastCpuTime) / 5000),
            io:fwrite("\n"),
            apply_after(5000, main, start_perf_analyzer, [CpuTime, Master_PID])
    end.

start_master(AmountOfCoins, LeadingZerosForCoin) ->
    % Here to change the amount of worker nodes used:
    AmountOfWorkerNodes = 5,
    register(master, spawn(main, master, [main, AmountOfWorkerNodes, AmountOfCoins, LeadingZerosForCoin, 0, erlang:timestamp()])).
