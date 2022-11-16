% @author Mathias Brekkan and Ruiyang Li
-record(tweet, {Text, Hashtags, Mentions, OriginalTweeter, ActualTweeter}). 
-record(user, {Username, Pid, Tweets, FollowersList, FollowedHashTags, MentionTweets, FollowingUsersTweets, IsLoggedOn}).