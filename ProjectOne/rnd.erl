-module(rnd).

-export([rnd_chars/1, rnd_numbers/1, rnd_chars_numbers/1]).

rnd_chars(L)         -> get_rnd(L, chars).
rnd_numbers(L)       -> get_rnd(L, numbers).
rnd_chars_numbers(L) -> get_rnd(L, chars_numbers).

get_rnd(L, chars)         -> gen_rnd(L, "abcdefghijklmnopqrstuvwxyz");
get_rnd(L, numbers)       -> gen_rnd(L, "1234567890");
get_rnd(L, chars_numbers) -> gen_rnd(L, "abcdefghijklmnopqrstuvwxyz1234567890").

gen_rnd(Length, AllowedChars) ->
  MaxLength = length(AllowedChars),
  lists:foldl(
    fun(_, Acc) -> [lists:nth(crypto:rand_uniform(1, MaxLength), AllowedChars)] ++ Acc end,
    [], lists:seq(1, Length)
  ).