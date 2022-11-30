% @author Mathias Brekkan and Ruiyang Li

-module(engine).
-export([startEngine/0, engineTick/6, sendLiveTweets/3]).
-include("records.hrl"). 

% Engine concepts
% Register account
% Send tweet, with hashtags and mentions
% Subscribe to user's tweets
% Re-tweets
% Handle mentions, hashtag subscriptions, followers
%  - Live if user is connected
%  - Nonlive/query if user is not connected

startEngine() ->
    io:fwrite("Starting engine~n"),
    Pid = spawn(engine, engineTick, [#{}, #{}, #{}, #{}, #{}, #{}]),
    io:format("~w~n", [Pid]),
    register(engine, Pid).

engineTick(Users, ActiveUsers, UserFollowersMap, UserRecievedTweetsMap, UserSentTweetsMap, HashTagSubscriptions) ->
    io:format("Users: ~w~n", [Users]),
    io:format("ActiveUsers: ~w~n", [ActiveUsers]),
    io:format("UserFollowersMaps: ~w~n", [UserFollowersMap]),
    io:format("HashTagSubscriptions: ~w~n", [HashTagSubscriptions]),
    receive
        {register, Username} ->
            io:format("User registered~n"),
            NewUser = #user{username = Username},
            NewUsers = maps:put(Username, NewUser, Users),
            engineTick(NewUsers, ActiveUsers, UserFollowersMap, UserRecievedTweetsMap, UserSentTweetsMap, HashTagSubscriptions);
        {logIn, Username, Pid} ->
            io:format("User logged in~n"),
            NewActiveUsers = maps:put(Username, Pid, ActiveUsers),
            TweetsRecieved = maps:get(Username, UserRecievedTweetsMap, []),
            Pid ! {recieveQuery, TweetsRecieved},
            engineTick(Users, NewActiveUsers, UserFollowersMap, UserRecievedTweetsMap, UserSentTweetsMap, HashTagSubscriptions);
        {logOut, Username} ->
            io:format("User logged out~n"),
            NewActiveUsers = maps:remove(Username, ActiveUsers),
            engineTick(Users, NewActiveUsers, UserFollowersMap, UserRecievedTweetsMap, UserSentTweetsMap, HashTagSubscriptions);
        {followUser, MyUsername, FollowThisUsername} ->
            io:format("User followed user~n"),
            UserFollowersEntry = maps:get(FollowThisUsername, UserFollowersMap, []),
            NewUserFollowersMap = maps:put(FollowThisUsername, [UserFollowersEntry | MyUsername], UserFollowersMap),
            engineTick(Users, ActiveUsers, NewUserFollowersMap, UserRecievedTweetsMap, UserSentTweetsMap, HashTagSubscriptions);
        {followHashTag, MyUsername, HashTag} ->
            io:format("User followed hashtag~n"),
            HashTagsEntry = maps:get(HashTag, HashTagSubscriptions, []),
            NewHashTagSubscriptions = maps:put(HashTag, [HashTagsEntry | MyUsername], HashTagSubscriptions),
            engineTick(Users, ActiveUsers, UserFollowersMap, UserRecievedTweetsMap, UserSentTweetsMap, NewHashTagSubscriptions);
        % Tweet: text, hashtags, mentions, originalTweeter, actualTweeter
        {sendTweet, Username, Tweet} ->
            io:format("Receved new tweet~n"),
            TweetsSentByUser = maps:get(Username, UserSentTweetsMap, []),
            NewUserSentTweetsMap = maps:put(Username, [TweetsSentByUser | Tweet], UserSentTweetsMap),
            Followers = maps:get(Username, UserFollowersMap, []),
            MentionedUsers = Tweet#tweet.mentions,
            FollowersOfHashTags = getAllUsersFromHashTags(HashTagSubscriptions, Tweet#tweet.hashTags, []),
            AllUsersNeedingTweetRaw = lists:append(lists:append(lists:append(Followers, MentionedUsers), FollowersOfHashTags), [Tweet#tweet.originalTweeter, Tweet#tweet.actualTweeter]).
            AllUsersNeedingTweet = sets:to_list(sets:from_list(AllUsersNeedingTweetRaw)),
            
            NewUserRecievedTweetsMap = updateRecievedTweetMap(Tweet, UserRecievedTweetsMap, AllUsersNeedingTweet),
            
            spawn(engine, sendLiveTweets, [Tweet, ActiveUsers, AllUsersNeedingTweet]),
            engineTick(Users, ActiveUsers, UserFollowersMap, NewUserRecievedTweetsMap, NewUserSentTweetsMap, HashTagSubscriptions)
    end.

sendLiveTweets(Tweet, ActiveUsers, [CurrentUserNeedingTweet, RemaningUsersToRecieveTweets]) ->
    CurrentActiveUserPid = maps:get(CurrentUserNeedingTweet, ActiveUsers, nil),
    if
        CurrentActiveUserPid == nil ->
            ok;
        true ->
            CurrentActiveUserPid ! {publishTweet, Tweet}
    end,
    sendLiveTweets(Tweet, ActiveUsers, RemaningUsersToRecieveTweets).

updateRecievedTweetMap(Tweet, UserRecievedTweetsMap, []) ->
    UserRecievedTweetsMap;
updateRecievedTweetMap(Tweet, UserRecievedTweetsMap, [CurrentUser, RemaningUsersToRecieveTweets]) ->
    CurrentRecievedTweets = maps:get(CurrentUser, UserRecievedTweetsMap, []),
    NewUserRecievedTweetsMap = maps:put(CurrentUser, [CurrentRecievedTweets | Tweet]),
    updateRecievedTweetMap(Tweet, NewUserRecievedTweetsMap, RemaningUsersToRecieveTweets).

getAllUsersFromHashTags(HashTagSubscriptions, [], AllFollowers) ->
    AllFollowers;
getAllUsersFromHashTags(HashTagSubscriptions, [HashTag, RemainingHashTags], AllFollowers) ->
    UsersFollowingHashTag = maps:get(HashTag, HashTagSubscriptions),
    getAllUsersFromHashTags(HashTagSubscriptions, RemainingHashTags, list:append(AllFollowers, UsersFollowingHashTag)).
