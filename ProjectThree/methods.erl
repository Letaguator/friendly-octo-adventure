-module(methods).
-export([getRandomNumber/2, getRandomString/1, getHash/1, adjustToLinearBounds/2, getM/0]).

getRandomNumber(Min, Max) ->
    crypto:rand_uniform(Min, Max + 1).

getRandomString(Length) ->
    AllowedChars = "abcdefghijklmnopqrstuvwxyz1234567890",
    MaxLength = length(AllowedChars),
    lists:foldl(
        fun(_, Acc) -> [lists:nth(crypto:rand_uniform(1, MaxLength), AllowedChars)] ++ Acc end,
        [], lists:seq(1, Length)
    ).


getM() ->
    16.


getHash(Key) ->
    binary:decode_unsigned(crypto:hash(sha, Key)) rem round(math:pow(2, getM())).

adjustToLinearBounds(TargetIndex, Count) ->
    if
        TargetIndex > Count ->
            TargetIndex - Count;
        TargetIndex < 1 ->
            Count + TargetIndex;
        true ->
            TargetIndex
    end.