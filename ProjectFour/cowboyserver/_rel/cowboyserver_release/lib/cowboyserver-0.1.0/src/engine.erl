% @author Mathias Brekkan and Ruiyang Li

-module(engine).
-export([startEngine/0, engineTick/12, sendLiveTweets/3]).
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
    register(engine, spawn(engine, engineTick, [erlang:timestamp(), erlang:timestamp(), 0, 0, #{}, #{}, #{}, #{}, #{}, #{}, #{}, #{}])).

engineTick(StartTime, CurTime, TweetSent, RetweetSent, Users, ActiveUsers, UserFollowersMap, UserRecievedTweetsMap, UserSentTweetsMap, HashTagSubscriptions, MentionedTweetsMap, HashtagTweetsMap) ->
    io:format("Time: ~w~n", [timer:now_diff(CurTime, StartTime) / 1000000]),
    io:format("Users: ~w~n", [maps:size(Users)]),
    io:format("ActiveUsers: ~w~n", [maps:size(ActiveUsers)]),
    io:format("Tweet Sent: ~w~n", [TweetSent]),
    io:format("Retweet Sent: ~w~n", [RetweetSent]),
    
    % io:format("UserFollowersMaps: ~w~n", [UserFollowersMap]),
    % io:format("HashTagSubscriptions: ~w~n", [HashTagSubscriptions]),

    receive
        {register, Username} ->
            io:format("User registered~n"),
            NewUser = #user{username = Username},
            NewUsers = maps:put(Username, NewUser, Users),
            engineTick(StartTime, erlang:timestamp(), TweetSent, RetweetSent, NewUsers, ActiveUsers, UserFollowersMap, UserRecievedTweetsMap, UserSentTweetsMap, HashTagSubscriptions, MentionedTweetsMap, HashtagTweetsMap);
        {query, Username, Pid} ->
            io:format("User queried in~n"),
            TweetsRecieved = maps:get(Username, UserRecievedTweetsMap, []),
            Pid ! {recieveQuery, TweetsRecieved},
            engineTick(StartTime, erlang:timestamp(), TweetSent, RetweetSent, Users, ActiveUsers, UserFollowersMap, UserRecievedTweetsMap, UserSentTweetsMap, HashTagSubscriptions, MentionedTweetsMap, HashtagTweetsMap);
        {queryMention, _Username, Pid, Mention} ->
            io:format("User queried mention in~n"),
            MentionTweets = maps:get(Mention, MentionedTweetsMap, []),
            Pid ! {recieveQueryMention, MentionTweets},
            engineTick(StartTime, erlang:timestamp(), TweetSent, RetweetSent, Users, ActiveUsers, UserFollowersMap, UserRecievedTweetsMap, UserSentTweetsMap, HashTagSubscriptions, MentionedTweetsMap, HashtagTweetsMap);
        {queryHashtag, _Username, Pid, Hashtag} ->
            io:format("User queried hashtag in~n"),
            HashtagTweets = maps:get(Hashtag, HashtagTweetsMap, []),
            Pid ! {recieveQueryHashtag, HashtagTweets},
            engineTick(StartTime, erlang:timestamp(), TweetSent, RetweetSent, Users, ActiveUsers, UserFollowersMap, UserRecievedTweetsMap, UserSentTweetsMap, HashTagSubscriptions, MentionedTweetsMap, HashtagTweetsMap);
        {logIn, Username, Pid} ->
            io:format("User logged in~n"),
            NewActiveUsers = maps:put(Username, Pid, ActiveUsers),
            TweetsRecieved = maps:get(Username, UserRecievedTweetsMap, []),
            Pid ! {recieveQuery, TweetsRecieved},
            engineTick(StartTime, erlang:timestamp(), TweetSent, RetweetSent, Users, NewActiveUsers, UserFollowersMap, UserRecievedTweetsMap, UserSentTweetsMap, HashTagSubscriptions, MentionedTweetsMap, HashtagTweetsMap);
        {logOut, Username} ->
            io:format("User logged out~n"),
            NewActiveUsers = maps:remove(Username, ActiveUsers),
            engineTick(StartTime, erlang:timestamp(), TweetSent, RetweetSent, Users, NewActiveUsers, UserFollowersMap, UserRecievedTweetsMap, UserSentTweetsMap, HashTagSubscriptions, MentionedTweetsMap, HashtagTweetsMap);
        {followUser, MyUsername, FollowThisUsername} ->
            io:format("User followed user~n"),
            UserFollowersEntry = maps:get(FollowThisUsername, UserFollowersMap, []),
            NewUserFollowersMap = maps:put(FollowThisUsername, [MyUsername | UserFollowersEntry], UserFollowersMap),
            engineTick(StartTime, erlang:timestamp(), TweetSent, RetweetSent, Users, ActiveUsers, NewUserFollowersMap, UserRecievedTweetsMap, UserSentTweetsMap, HashTagSubscriptions, MentionedTweetsMap, HashtagTweetsMap);
        {followHashTag, MyUsername, HashTag} ->
            io:format("User followed hashtag~n"),
            HashTagsEntry = maps:get(HashTag, HashTagSubscriptions, []),
            NewHashTagSubscriptions = maps:put(HashTag, [MyUsername | HashTagsEntry], HashTagSubscriptions),
            engineTick(StartTime, erlang:timestamp(), TweetSent, RetweetSent, Users, ActiveUsers, UserFollowersMap, UserRecievedTweetsMap, UserSentTweetsMap, NewHashTagSubscriptions, MentionedTweetsMap, HashtagTweetsMap);
        {sendTweet, Username, Tweet} ->
            io:format("Recieved new tweet~n"),
            TweetsSentByUser = maps:get(Username, UserSentTweetsMap, []),
            NewUserSentTweetsMap = maps:put(Username, [TweetsSentByUser | Tweet], UserSentTweetsMap),
            Followers = maps:get(Username, UserFollowersMap, []),
            MentionedUsers = Tweet#tweet.mentions,
            FollowersOfHashTags = getAllUsersFromHashTags(HashTagSubscriptions, Tweet#tweet.hashTags, []),
            AllUsersNeedingTweetRaw = Followers ++ MentionedUsers ++ FollowersOfHashTags ++ [Tweet#tweet.originalTweeter, Tweet#tweet.actualTweeter],
            AllUsersNeedingTweet = sets:to_list(sets:from_list(AllUsersNeedingTweetRaw)),
            
            NewUserRecievedTweetsMap = updateRecievedTweetMap(Tweet, UserRecievedTweetsMap, AllUsersNeedingTweet),
            NewMentionedTweetsMap = updateMentionedTweetMap(Tweet, MentionedTweetsMap, MentionedUsers),
            NewHashtagTweetsMap = updateHashtagTweetMap(Tweet, HashtagTweetsMap, Tweet#tweet.hashTags),
            
            spawn(engine, sendLiveTweets, [Tweet, ActiveUsers, AllUsersNeedingTweet]),

            if
                Tweet#tweet.actualTweeter == Tweet#tweet.originalTweeter ->
                    engineTick(StartTime, erlang:timestamp(), TweetSent + 1, RetweetSent + 1, Users, ActiveUsers, UserFollowersMap, NewUserRecievedTweetsMap, NewUserSentTweetsMap, HashTagSubscriptions, NewMentionedTweetsMap, NewHashtagTweetsMap);
                true ->
                    engineTick(StartTime, erlang:timestamp(), TweetSent + 1, RetweetSent, Users, ActiveUsers, UserFollowersMap, NewUserRecievedTweetsMap, NewUserSentTweetsMap, HashTagSubscriptions, NewMentionedTweetsMap, NewHashtagTweetsMap)
            end
    end.

sendLiveTweets(_, _, []) ->
    ok;
sendLiveTweets(Tweet, ActiveUsers, [CurrentUserNeedingTweet | RemaningUsersToRecieveTweets]) ->
    CurrentActiveUserPid = maps:get(CurrentUserNeedingTweet, ActiveUsers, nil),
    if
        CurrentActiveUserPid == nil ->
            ok;
        true ->
            CurrentActiveUserPid ! {publishTweet, Tweet}
    end,
    sendLiveTweets(Tweet, ActiveUsers, RemaningUsersToRecieveTweets).

updateMentionedTweetMap(_Tweet, MentionedTweetsMap, []) ->
    MentionedTweetsMap;
updateMentionedTweetMap(Tweet, MentionedTweetsMap, [MentionedUser | RemainingMentionedUsers]) ->
    MentionedTweets = maps:get(MentionedUser, MentionedTweetsMap, []),
    NewMentionedTweetsMap = maps:put(MentionedUser, [Tweet | MentionedTweets], MentionedTweetsMap),
    updateMentionedTweetMap(Tweet, NewMentionedTweetsMap, RemainingMentionedUsers).

updateHashtagTweetMap(_Tweet, HashtagTweetsMap, []) ->
    HashtagTweetsMap;
updateHashtagTweetMap(Tweet, HashtagTweetsMap, [Hashtag | RemainingHashtags]) ->
    HashtagTweets = maps:get(Hashtag, HashtagTweetsMap, []),
    NewHashtagTweetsMap = maps:put(Hashtag, [Tweet | HashtagTweets], HashtagTweetsMap),
    updateHashtagTweetMap(Tweet, NewHashtagTweetsMap, RemainingHashtags).

updateRecievedTweetMap(_, UserRecievedTweetsMap, []) ->
    UserRecievedTweetsMap;
updateRecievedTweetMap(Tweet, UserRecievedTweetsMap, [CurrentUser | RemaningUsersToRecieveTweets]) ->
    CurrentRecievedTweets = maps:get(CurrentUser, UserRecievedTweetsMap, []),
    NewUserRecievedTweetsMap = maps:put(CurrentUser, [Tweet | CurrentRecievedTweets], UserRecievedTweetsMap),
    updateRecievedTweetMap(Tweet, NewUserRecievedTweetsMap, RemaningUsersToRecieveTweets).

getAllUsersFromHashTags(_, [], AllFollowers) ->
    AllFollowers;
getAllUsersFromHashTags(HashTagSubscriptions, [HashTag | RemainingHashTags], AllFollowers) ->
    UsersFollowingHashTag = maps:get(HashTag, HashTagSubscriptions, []),
    getAllUsersFromHashTags(HashTagSubscriptions, RemainingHashTags, AllFollowers ++ UsersFollowingHashTag).
