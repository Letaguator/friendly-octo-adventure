{application, 'cowboyserver', [
	{description, "Cowboy WebSocket server"},
	{vsn, "0.1.0"},
	{modules, ['cowboyserver_app','cowboyserver_sup']},
	{registered, [cowboyserver_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {cowboyserver_app, []}},
	{env, []}
]}.