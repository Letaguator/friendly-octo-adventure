-module(ws_handler).
-behavior(cowboy_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).
-include("records.hrl"). 

init(Req, State) ->
	{cowboy_websocket, Req, State}.

websocket_init(State) ->
    {reply, {text, <<"Hello!">>}, State}.

% Recieve data from client
websocket_handle({text, Msg}, State) ->
	io:fwrite("websocket_handle\n"),
	ParsedData = string:lexemes(Msg, "|"),
	io:fwrite("lexems done\n"),
	handleRequestCaller(State, ParsedData),
	io:fwrite("handleRequest done\n"),
    {reply, {text, Msg}, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

handleRequestCaller(State, [ MessageType | MessageTokens]) ->
	StrMessageType = binary_to_list(MessageType),
	handleRequest(State, StrMessageType, MessageTokens).

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
		Tweet = #tweet{text = Message, hashTags = [], mentions = [], originalTweeter = OriginalTweeter, actualTweeter = MyUsername},
		engine ! {sendTweet, MyUsername, Tweet};
handleRequest(_, _, _) ->
			io:fwrite("Unknown message recieved\n").

formatTweetsForSending(CurrentMessage, []) ->
	CurrentMessage;
formatTweetsForSending(CurrentMessage, [CurrentTweet | RemainingTweets]) ->
	io:fwrite("!!FormatBegin\n"),
	AddToMessage = formatTweetForSending(CurrentTweet),
	io:fwrite("!!FormatNext\n"),
	formatTweetsForSending(CurrentMessage ++ AddToMessage, RemainingTweets).

formatTweetForSending(Tweet) ->
	Result = binary_to_list(Tweet#tweet.actualTweeter) ++ "|" ++ binary_to_list(Tweet#tweet.originalTweeter) ++ "|" ++ binary_to_list(Tweet#tweet.text) ++ "^",
	Result.

% Recieve messages from the engine
websocket_info({recieveQuery, Data}, State) ->
	io:fwrite("!!recieveQuery\n"),
	SendTweets = list_to_binary(formatTweetsForSending("", Data)),
	io:fwrite("!!FormatOver\n"),
	{reply, {text, SendTweets}, State};
websocket_info({publishTweet, Data}, State) ->
	io:fwrite("!!publishTweet\n"),
	io:write(Data),
	SendTweet = list_to_binary(formatTweetsForSending("", [Data])),
	io:fwrite("!!FormatOver\n"),
	{reply, {text, SendTweet}, State};
websocket_info(_Request, _State) ->
	ok.