% @author Mathias Brekkan and Ruiyang Li
-module(simulator).
-export([zipf/3, startSim/1]).

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

getUsernamesList(0, UsernamesList) ->
    UsernamesList;
getUsernamesList(NumberOfUsers, UsernamesList) ->
    Username = getRandomString(16),
    getUsernamesList(NumberOfUsers - 1, [Username | UsernamesList]).

startSim(NumberOfUsers) ->
    Usernames = getUsernamesList(NumberOfUsers, []),
    io:write(Usernames).
% Create a list of N usernames
% Give every user a random amount of subscribers 
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