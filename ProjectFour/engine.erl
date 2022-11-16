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
    engineTick({}, {}).

engineTick(Users, HashTagSubscriptions) ->
    receive
        {register, Username} ->
            ok;
        {logIn, Username, Pid} ->
            ok;
        {logOff, Username} ->
            ok;
        {sendTweet, Username} ->
            ok;
    end.