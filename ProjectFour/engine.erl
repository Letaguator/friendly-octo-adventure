% @author Mathias Brekkan and Ruiyang Li

-module(main).
-export([engineTick/1]).
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
    io:fwrite("Starting engine"),
    engineTick({}, {}, {}).

engineTick(Users, ActiveUsers, UserFollowersMap, HashTagSubscriptions) ->
    receive
        {register, Username} ->
            NewUser = #user{username = Username, pid = nil, tweets = [], followersList = [], followedHashTagTweet = [], mentionTweets = [], followingUsersTweets = [], isLoggedOn = false},
            NewUsers = maps:put(Username, NewUser, Users),
            engineTick(NewUsers, ActiveUsers, UserFollowersMap, HashTagSubscriptions);
        {logIn, Username, Pid} ->
            NewActiveUsers = maps:put(Username, Pid, ActiveUsers),
            engineTick(Users, NewActiveUsers, UserFollowersMap, HashTagSubscriptions);
        {logOut, Username} ->
            NewActiveUsers = map:remove(Username, ActiveUsers),
            engineTick(Users, NewActiveUsers, UserFollowersMap, HashTagSubscriptions);
        {followUser, MyUsername, FollowThisUsername} ->
            UserFollowersEntry = map:get(FollowThisUsername, UserFollowersMap, []),
            NewUserFollowersMap = map:put(FollowThisUser, [UserFollowersEntry | MyUsername]),
            engineTick(Users, ActiveUsers, NewUserFollowersMap, HashTagSubscriptions);
        {followHashTag, MyUsername, HashTag} ->
            HashTagsEntry = maps:get(HashTag, HashTagSubscriptions, []),
            NewHashTagSubscriptions = maps:put(HashTag, [HashTagsEntry | MyUsername]),
            engineTick(NewUserUsers, ActiveUsers, UserFollowersMap, NewHashTagSubscriptions);
        {sendTweet, Username, Tweet} ->
            ok
    end.