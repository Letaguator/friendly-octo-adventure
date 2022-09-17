%% @author ruiyang
%% @doc @todo Add 

-module(main).

-import(string, [len/1, equal/2, concat/2, chr/2, substr/3, str/2, to_lower/1, to_upper/1 ]).
-import(rnd, [rnd_chars_numbers/1]).
-import(binary, [decode_unsigned/1]).
-import(crypto, [hash/1]).




-export([main/3, mining/2]).


main(Cores, 0, N) ->
    N,
    done;
main(Cores, Times, N) ->
    spawn(main, mining, [Times, N]),
    main(Cores - 1, Times, N).



mining(0, N) ->
    N,
    done;

mining(Times, N) ->
    Key = concat("liruiyang;", rnd:rnd_chars_numbers(10)),
    Hash = binary:decode_unsigned(crypto:hash(sha256, rnd_chars_numbers(10))),
    case Hash < math:pow(16, 64 - N) of
        true ->
            io:format("~p:", [Key]),
            io:format("~64.16.0b~n", [Hash]),
            mining(Times - 1, N);
        false ->
            mining(Times, N)
    end.
    




