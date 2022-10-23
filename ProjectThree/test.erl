-module(test).


-import(methods, [getRandomNumber/2, getRandomString/1, getHash/1, adjustToLinearBounds/2]).


-export([printList/1, testHash/1]).



printList(List) ->
     io:fwrite("~w~n",[List]).





testHash(Key) ->
    io:format("~p", [getHash(Key)]).





