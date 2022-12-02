% @author Mathias Brekkan and Ruiyang Li
-module(simulator).
-export([zipf/3, startSim/1, startSimLifecycle/3]).
-include("records.hrl"). 
-import(user, [printList/1, query/1, register/0, reTweet/4, logIn/1, logOut/0, sendTweet/3, client/2, client/3, followUser/1, reg/1, followHashTag/1]).

% Formula from http://www.math.wm.edu/~leemis/chart/UDR/PDFs/Zipf.pdf
% In our case x can be number of subscribers, n can be maximum amount of subscribers
% a can be 1 for now
zipf(X, A, N) ->
    1 / (math:pow(X, A) * zipfSumPart(1, A, N, 0)).

zipfSumPart(N, _, N, Res) ->
    Res;
zipfSumPart(I, A, N, Res) ->
    zipfSumPart(I + 1, A, N, Res + math:pow(1 / I, A)).

getRandomString(Length) ->
    AllowedChars = "abcdefghijklmnopqrstuvwxyz1234567890",
    MaxLength = length(AllowedChars),
    lists:foldl(
        fun(_, Acc) -> [lists:nth(crypto:rand_uniform(1, MaxLength), AllowedChars)] ++ Acc end,
        [], lists:seq(1, Length)
    ).

getRandomNumber(Min, Max) ->
    crypto:rand_uniform(Min, Max + 1).

getUsernamesList(0, UsernamesList, _) ->
    UsernamesList;
getUsernamesList(NumberOfUsers, UsernamesList, NumberOfTotalUsers) ->
    Username = getRandomString(16),
    UserPop = #userPop{username = Username, popularity = zipf(NumberOfUsers, 3, NumberOfTotalUsers)},
    getUsernamesList(NumberOfUsers - 1, [UserPop | UsernamesList], NumberOfTotalUsers).

allocateSubscribers(Users, [], NumberOfUsers, SubscriberMap) ->
    SubscriberMap;
allocateSubscribers(Users, [CurrentUser | UserLeft], NumberOfUsers, SubscriberMap) ->
    Username = CurrentUser#userPop.username,
    UserPop = CurrentUser#userPop.popularity,
    TotalSubscriberCount = round(UserPop * NumberOfUsers),
    RandomIndex = getRandomNumber(1, NumberOfUsers),
    NewSubscriberMap = findSubscribersForUser(Username, Users, RandomIndex, TotalSubscriberCount, SubscriberMap),
    allocateSubscribers(Users, UserLeft, NumberOfUsers, NewSubscriberMap).

findSubscribersForUser(CurrentUsername, Usernames, IndexToFetchUsersFrom, 0, SubscriberMap) ->
    SubscriberMap;
findSubscribersForUser(CurrentUsername, Users, IndexToFetchUsersFrom, TotalSubscriberCount, SubscriberMap) ->
    CurrentUserWhoWillSubscribe = lists:nth(IndexToFetchUsersFrom, Users),
    UsernameWhoWillSubscribe = CurrentUserWhoWillSubscribe#userPop.username,
    UsersUserWillSubscribeTo = maps:get(UsernameWhoWillSubscribe, SubscriberMap, []),
    NewSubscriberMap = maps:put(UsernameWhoWillSubscribe, UsersUserWillSubscribeTo ++ [CurrentUsername], SubscriberMap),
    NextIndex = IndexToFetchUsersFrom + 1,
    if
        NextIndex > length(Users) ->
            findSubscribersForUser(CurrentUsername, Users, 1, TotalSubscriberCount - 1, NewSubscriberMap);
        true ->
            findSubscribersForUser(CurrentUsername, Users, NextIndex, TotalSubscriberCount - 1, NewSubscriberMap)
    end.

startSim(NumberOfUsers) ->

    Usernames = getUsernamesList(NumberOfUsers, [], NumberOfUsers),
    SubscriberMap = allocateSubscribers(Usernames, Usernames, NumberOfUsers, #{}),
    startUsers(Usernames, SubscriberMap).

startUsers([], SubscriberMap) ->
    ok;
startUsers([CurrentUser | UsernamesLeft], SubscriberMap) ->
    Username = CurrentUser#userPop.username,
    UserPop = CurrentUser#userPop.popularity,
    UsersToSubscribeTo = maps:get(Username, SubscriberMap, []),
    spawn(simulator, startSimLifecycle, [Username, UserPop, UsersToSubscribeTo]),
    startUsers(UsernamesLeft, SubscriberMap).

subscribeToAllDesignatedUsers(_, []) ->
    ok;
subscribeToAllDesignatedUsers(Username, [UserToFollow | UsersLeftToFollow]) ->
    io:format("~w ", [Username]),
    followUser(UserToFollow),
    %io:format("||| ~s subscribed to ", [Username]),
    %io:format("~s~n", [UserToFollow]),
    subscribeToAllDesignatedUsers(Username, UsersLeftToFollow).

% register/0, reTweet/4, logIn/1, logOut/0, sendTweet/3, client/2, client/3, followUser/1, reg/1, followHashTag/1
startSimLifecycle(Username, UserPop, SubscriberList) ->
    
    %io:format("~w ~w ~n", [Username, SubscriberList]),
    reg(Username),
    logIn(Username),
    subscribeToAllDesignatedUsers(Username, SubscriberList),
    simLifecycle(Username, UserPop).
simLifecycle(Username, UserPop) ->
    LogOutProb = random:uniform(),
    if
        LogOutProb < UserPop/5 ->
            logOut(),
            SleepTime = round(50000 * random:uniform() * (1 - UserPop)),
            timer:sleep(SleepTime),
            logIn(Username);
        true ->
            ok
    end,

    timer:sleep(1000),
    TweetProbability = random:uniform(),
    if
        TweetProbability < UserPop ->
            
            sendTweet("Message", [], []);

        true ->
            ok
    end,
    
    % send retweets periodically

    simLifecycle(Username, UserPop).
% For every user with subscriber S, make S users take their username as input for subscription
% Give every user a online/offline behaviour, determining how often they connect/disconnect, zipf
% Give every user a tweet frequency rate, the higher the subscriber count S, the higher the rate TFR, zipf
% Give every user a retweet rate, the higher the subscriber count S, the higher the retweet rate RR, zipf
% Start simulation

% Measure number of tweets pr. second
% Measure number max amount of users during simulation
% Measure number Y
% Measure number Z
% Measure number ?
% Measure number ?
% Measure number ?