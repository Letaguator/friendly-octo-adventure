% @author Mathias Brekkan and Ruiyang Li

-module(engine).
-export([startEngine/0, engineTick/4]).
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
    Pid = spawn(engine, engineTick, [#{}, #{}, #{}, #{}]),
    register(engine, Pid).

engineTick(Users, ActiveUsers, UserFollowersMap, HashTagSubscriptions) ->
    io:format("Users: ~w~n", [Users]),
    io:format("ActiveUsers: ~w~n", [ActiveUsers]),
    io:format("UserFollowersMap: ~w~n", [UserFollowersMap]),
    io:format("HashTagSubscriptions: ~w~n", [HashTagSubscriptions]),
    receive
        {register, Username} ->
            NewUser = #user{username = Username, tweets = [], followedHashTagTweet = [], mentionTweets = [], followingUsersTweets = []},
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
            NewUserFollowersMap = map:put(FollowThisUsername, [UserFollowersEntry | MyUsername]),
            engineTick(Users, ActiveUsers, NewUserFollowersMap, HashTagSubscriptions);
        {followHashTag, MyUsername, HashTag} ->
            HashTagsEntry = maps:get(HashTag, HashTagSubscriptions, []),
            NewHashTagSubscriptions = maps:put(HashTag, [HashTagsEntry | MyUsername]),
            engineTick(Users, ActiveUsers, UserFollowersMap, NewHashTagSubscriptions);
        {sendTweet, Username, Tweet} ->
            ok
    end.