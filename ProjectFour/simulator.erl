% @author Mathias Brekkan and Ruiyang Li

% Formula from http://www.math.wm.edu/~leemis/chart/UDR/PDFs/Zipf.pdf
zipf(x, a, n) ->
    1/(math:pow(x, a) * zipfSumPart(1, a, n, 0)).
zipfSumPart(n, a, n, res) ->
    res;
zipfSumPart(i, a, n, res) ->
    zipfSumPart(i + 1, a, n, res + math:pow(1/i, a)).


% Take in number N of users to simulate
% Create a list of N usernames
% Give every user a random amount of subscribers 
% For every user with subscriber S, make S users take their username as input for subscription
% Give every user a online/offline behaviour, determining how often they connect/disconnect, zipf
% Give every user a tweet frequency rate, the higher the subscriber count S, the higher the rate TFR, zipf
% Give every user a retweet rate, the higher the subscriber count S, the higher the retweet rate RR, zipf
% Start simulation

% Measure number of tweets pr. second
% Measure number X
% Measure number Y
% Measure number Z
% Measure number ?
% Measure number ?
% Measure number ?