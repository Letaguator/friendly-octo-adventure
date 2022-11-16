% @author Mathias Brekkan and Ruiyang Li
-record(tweet, {text, hashtags, mentions}). 
-record(user, {username, tweets, mentions, followingUsers, subscriptions}).