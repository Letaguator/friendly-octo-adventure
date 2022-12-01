## Project 4

Mathias Brekkan
Ruiyang Li

### Important
For a more in-depth look, it is recommended to look at the report, as this README.MD is not exhaustive.

### Compilation & Running

Compiling code:
- c(engine), c(user), c(simulator).
- c(main). or c(main, [debug_info]).
- c(methods), c(test), c(main).


To run the code:

start a master node by `erl -sname master`
on the master node run `c(engine), engine:startEngine().`

start a user node by `erl -sname <some user name>`
ont he user node run `c(user), user:reg("<username>"), user:logIn("<username>").`



other functionalities:
```
user:sendTweet(Message, Hashtags, Mentions)
user:followUser(FollowThisUsername)
user:followHashTag(FollowThisHashTag)
user:logOff()
user:reTweet(Message, Hashtags, Mentions, OG)

```



### Code snippets
Code snippets can be found in the report