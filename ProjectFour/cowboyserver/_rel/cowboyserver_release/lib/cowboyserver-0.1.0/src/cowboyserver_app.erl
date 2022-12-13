-module(cowboyserver_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
        {'_', [
			{"/", cowboy_static, {priv_file, cowboyserver, "/static/index.html"}},
			{"/hello", hello_handler, []},
        	{"/websocket", ws_handler, []}
		]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
	engine:startEngine(),
	cowboyserver_sup:start_link().

stop(_State) ->
	ok.
