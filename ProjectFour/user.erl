% @author Mathias Brekkan and Ruiyang Li
-include("records.hrl"). 


-module(user).
-export([register/0, reTweet/4, logOn/1, logOff/0, sendTweet/1]).



register() ->
    case whereis(mess_client) of % Test if the client is running
        undefined ->
            not_logged_on;
        _ -> 

            mess_client ! {register},
            ok
    end.



reTweet(Message, Hashtags, Mentions, OG) ->    
    case whereis(mess_client) of % Test if the client is running
        undefined ->
            not_logged_on;
        _ -> 
            Tweet = #tweet{text = Message, hashtags = Hashtags, mentions = Mentions, originalTweeter = OG, actualTweeter = UserName},
            mess_client ! {sendTweet, Tweet},
            ok
    end.


printList([]) ->
    done;
printList([Head | Tail]) ->
    io:format("~w~n", [Head]),
    printList(Tail).






%%% User Commands
logOn(UserName) ->
    case whereis(mess_client) of 
        undefined ->
            register(mess_client, 
                     spawn(messenger, client, [server_node(), UserName]));
        _ -> already_logged_on
    end.

logOff() ->
    mess_client ! logoff.



sendTweet(Message) ->
    case whereis(mess_client) of % Test if the client is running
        undefined ->
            not_logged_on;
        _ -> 
            Tweet = #tweet{text = Message, hashtags = Hashtags, mentions = Mentions, originalTweeter = UserName, actualTweeter = UserName},
            mess_client ! {sendTweet, Tweet},
            ok
    end.


%%% The client process which runs on each server node
client(Server_Node, UserName) ->
    Server_Node ! {self(), logon, UserName},
    await_result(),
    client(Server_Node, UserName, running).

client(Server_Node, UserName, running) ->
    receive

        register ->
            Server_Node ! {register, UserName};
        logoff ->
            Server_Node ! {logOff, UserName},
            exit(normal);

        {sendReTweet} ->
            Tweet = #tweet{text = Message, hashtags = Hashtags, mentions = Mentions, originalTweeter = OG, actualTweeter = UserName},
            Server_Node ! {sendTweet, UserName, Tweet};
        {sendTweet, Tweet} ->
            Server_Node ! {self(), message_to, ToName, Message},
            await_result();
        {tweet, FromName, Message} ->
            io:format("~w~n", [Tweet]);
        {query, Query} ->
            printList(Query)
    end,
    client(Server_Node, UserName, running).

