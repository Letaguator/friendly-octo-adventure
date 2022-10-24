-module(test).


-import(methods, [getRandomNumber/2, getRandomString/1, getHash/1, adjustToLinearBounds/2]).


-export([printList/1, testHash/1, testInterval/1]).



printList(List) ->
     io:fwrite("~w~n",[List]).


testInterval(1) ->
     Time = 100,
     Message = "aaa",
     timer:send_interval(Time, Message),
     {ok, data};

testInterval(handle) ->
     receive
          {data} ->
               ok
     end.



testHash(Key) ->
    io:format("~p", [getHash(Key)]).





