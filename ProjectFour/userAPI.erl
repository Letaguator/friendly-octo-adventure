% @author Mathias Brekkan and Ruiyang Li
-module(userAPI).
-include("records.hrl"). 

-export([spawnClient/1, query/1, register/0, reTweet/4, logIn/1, logOut/0, sendTweet/3, client/2, client/3, followUser/1, reg/1, followHashTag/1]).


server_node() ->
    % 'master@LAPTOP-M9SIRB3U'.
    'mast@Laptop-Waldur'.


%% NOT IN USE
%% NOT IN USE
register() ->
    case whereis(mess_client) of % Test if the client is running
        undefined ->
            not_logged_on;
        _ ->

            mess_client ! {register},
            ok
    end.
%% NOT IN USE
%% NOT IN USE



%% NOT IN USE
%% NOT IN USE
reTweet(Message, Hashtags, Mentions, OG) ->    
    mess_client ! {sendTweet, Message, Hashtags, Mentions, OG}.
%% NOT IN USE
%% NOT IN USE

printList([]) ->
    done;
printList([Head | Tail]) ->
    io:format("~s, ", [Head]),
    printList(Tail).


printQuery([]) ->
    done;
printQuery([Head | Tail]) ->
    printTweet(Head),
    printQuery(Tail).

printTweet(Tweet) ->
    io:format("~s recieved tweet~n", [Tweet#tweet.actualTweeter]),
    io:format("Originally posted by ~s~n", [Tweet#tweet.originalTweeter]),
    % io:format("#"),
    % printList(Tweet#tweet.hashTags),
    % io:format("~n"),
    % io:format("@"),
    % printList(Tweet#tweet.mentions),
    io:format("~n"),           
    io:format("~s~n", [Tweet#tweet.text]).

printTweet(Username, Tweet) ->
    io:format("~s recieved tweet: ~s from ~s ~n", [Username, Tweet#tweet.text, Tweet#tweet.actualTweeter]).
    % io:format("Originally posted by ~s~n", [Tweet#tweet.originalTweeter]),
    % io:format("#"),
    % printList(Tweet#tweet.hashTags),
    % io:format("~n"),
    % io:format("@"),
    % printList(Tweet#tweet.mentions),
    % io:format("~n"),           
    % io:format("~s~n", [Tweet#tweet.text]).

spawnClient(Username) -> 
    spawn(user, client, [server_node(), Username]).

%% NOT IN USE
%% NOT IN USE
%% NOT IN USE
%% NOT IN USE
%% NOT IN USE
reg(UserName) ->
    {engine, server_node()} ! {register, UserName}.

%% User Commands
logIn(UserName) ->
    {engine, server_node()} ! {logIn, UserName, whereis(mess_client)}.

query(UserName) ->
    {engine, server_node()} ! {query, UserName, whereis(mess_client)}.

logOut() ->
    mess_client ! logOut.

sendTweet(Message, Hashtags, Mentions) ->
    mess_client ! {sendTweet, Message, Hashtags, Mentions}.

followUser(FollowThisUsername) ->
    mess_client ! {followUser, FollowThisUsername}.

followHashTag(FollowThisHashTag) ->
    mess_client ! {followHashTag, FollowThisHashTag}.
%% NOT IN USE
%% NOT IN USE
%% NOT IN USE
%% NOT IN USE
%% NOT IN USE
%% NOT IN USE

%%% The client process which runs on each client node
client(Server_Node, UserName) ->
    {engine, Server_Node} ! {logIn, UserName, self()},
    io:format("login information sent~n"),
    client(Server_Node, UserName, running).

client(Server_Node, UserName, running) ->
    receive
        logOut ->
            {engine, Server_Node} ! {logOut, UserName},
            exit(normal);
        {sendReTweet, Message, Hashtags, Mentions, OG} ->
            Tweet = #tweet{text = Message, hashTags = Hashtags, mentions = Mentions, originalTweeter = OG, actualTweeter = UserName},
            {engine, Server_Node} ! {sendTweet, UserName, Tweet};
        {sendTweet, Message, Hashtags, Mentions} ->
            Tweet = #tweet{text = Message, hashTags = Hashtags, mentions = Mentions, originalTweeter = UserName, actualTweeter = UserName},
            {engine, Server_Node} ! {sendTweet, UserName, Tweet};
        {followUser, FollowThisUsername} ->
            io:format("~w ~w ~n", [UserName, FollowThisUsername]),
            {engine, Server_Node} ! {followUser, UserName, FollowThisUsername};
        {followHashTag, FollowThisHashTag} ->
            {engine, Server_Node} ! {followHashTag, UserName, FollowThisHashTag};
        {publishTweet, Tweet} ->
            printTweet(UserName, Tweet);
        {recieveQuery, Query} ->
            % Forward to sim last tweet
            printQuery(Query)
    end,
    client(Server_Node, UserName, running).

