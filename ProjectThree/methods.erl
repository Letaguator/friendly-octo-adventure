-module(methods).
-export([getRandomNumber/2, getRandomString/1, getHash/1, adjustToLinearBounds/2, getM/0, getCircleSize/0]).


getRandomNumber(Min, Max) ->
    crypto:rand_uniform(Min, Max + 1).

getRandomString(Length) ->
    AllowedChars = "abcdefghijklmnopqrstuvwxyz1234567890",
    MaxLength = length(AllowedChars),
    lists:foldl(
        fun(_, Acc) -> [lists:nth(crypto:rand_uniform(1, MaxLength), AllowedChars)] ++ Acc end,
        [], lists:seq(1, Length)
    ).

%%% This function makes M a global variable
getM() ->
    16.




getCircleSize() ->
    round(math:pow(2, getM())).

getHash(Key) ->
    HashedValue = crypto:hash(sha, Key),
    binary:decode_unsigned(HashedValue) rem round(math:pow(2, getM())).

adjustToLinearBounds(TargetIndex, Count) ->
    if
        TargetIndex > Count ->
            TargetIndex - Count;
        TargetIndex < 1 ->
            Count + TargetIndex;
        true ->
            TargetIndex
    end.