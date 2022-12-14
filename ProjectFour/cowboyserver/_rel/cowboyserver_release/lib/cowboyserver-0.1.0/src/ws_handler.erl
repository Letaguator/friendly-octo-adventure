-module(ws_handler).
-behavior(cowboy_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).
-include("records.hrl"). 
-import(string,[concat/2, substr/3]). 


init(Req, State) ->
	{cowboy_websocket, Req, State}.

websocket_init(State) ->
    {reply, {text, <<"Hello!">>}, State}.

% Recieve data from client
websocket_handle({text, Msg}, State) ->
	ParsedData = string:lexemes(Msg, "|"),
	handleRequestCaller(State, ParsedData),
    {reply, {text, Msg}, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

handleRequestCaller(State, [ MessageType | MessageTokens]) ->
	StrMessageType = binary_to_list(MessageType),
	StrMessageTokens = binListToStringList([], MessageTokens),
	printList(StrMessageTokens),
	handleRequest(State, StrMessageType, StrMessageTokens).

handleRequest(_State, "Ping", _MessageTokens) ->
	ok;
handleRequest(_State, "register", MessageTokens) ->
	Username = lists:nth(1, MessageTokens),
	engine ! {register, Username};
handleRequest(_State, "logIn", MessageTokens) ->
	Username = lists:nth(1, MessageTokens),
	engine ! {logIn, Username, self()};
handleRequest(_State, "logOut", MessageTokens) ->
	Username = lists:nth(1, MessageTokens),
	engine ! {logOut, Username};
handleRequest(_State, "followUser", MessageTokens) ->
	MyUsername = lists:nth(1, MessageTokens),
	Username = lists:nth(2, MessageTokens),
	engine ! {followUser, MyUsername, Username};
handleRequest(_State, "followHashtag", MessageTokens) ->
	MyUsername = lists:nth(1, MessageTokens),
	HashTag = lists:nth(2, MessageTokens),
	engine ! {followHashTag, MyUsername, HashTag};
handleRequest(_State, "sendTweet", MessageTokens) ->
	MyUsername = lists:nth(1, MessageTokens),
	OriginalTweeter = lists:nth(2, MessageTokens),
	Message = lists:nth(3, MessageTokens),
	io:fwrite("Before Hash\n"),
	HashTags = extract("#", Message),
	printList(HashTags),
	io:fwrite("Before Mention\n"),
	Mentions = extract("@", Message),
	printList(Mentions),
	io:fwrite("After all\n"),
	Tweet = #tweet{text = Message, hashTags = HashTags, mentions = Mentions, originalTweeter = OriginalTweeter, actualTweeter = MyUsername},
	engine ! {sendTweet, MyUsername, Tweet};
handleRequest(_, _, _) ->
	io:fwrite("Unknown message recieved\n").

formatTweetsForSending(CurrentMessage, []) ->
	CurrentMessage;
formatTweetsForSending(CurrentMessage, [CurrentTweet | RemainingTweets]) ->
	AddToMessage = formatTweetForSending(CurrentTweet),
	formatTweetsForSending(CurrentMessage ++ AddToMessage, RemainingTweets).

formatTweetForSending(Tweet) ->
	Result = Tweet#tweet.actualTweeter ++ "|" ++ Tweet#tweet.originalTweeter ++ "|" ++ Tweet#tweet.text ++ "^",
	Result.

% Recieve messages from the engine
websocket_info({recieveQuery, Data}, State) ->
	SendTweets = list_to_binary(formatTweetsForSending("", Data)),
	{reply, {text, SendTweets}, State};
websocket_info({publishTweet, Data}, State) ->
	io:write(Data),
	SendTweet = list_to_binary(formatTweetsForSending("", [Data])),
	{reply, {text, SendTweet}, State};
websocket_info(_Request, _State) ->
	ok.

extract(Cue, Str) ->
    case re:run(Str,concat(Cue, "[^@#\t\s\n$]+"), [global, notempty]) of 
        {match, Captured} -> 
            [substr(Str, element(1, lists:nth(1, Group)) + 2, element(2, lists:nth(1, Group)) - 1) || Group <- Captured, true];
        nomatch ->
            []
    end.

binListToStringList(ResultList, []) ->
	ResultList;
binListToStringList(ResultList, [BinListEntry | BinListRemaining]) ->
	binListToStringList(ResultList ++ [binary_to_list(BinListEntry)], BinListRemaining).

printList(List) ->
     io:fwrite("~w~n",[List]).