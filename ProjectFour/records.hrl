% @author Mathias Brekkan and Ruiyang Li
-record(tweet, {text, hashtags, mentions, originalTweeter, actualTweeter}). 
-record(user, {username, tweets, followedHashTagTweet, mentionTweets, followingUsersTweets}).