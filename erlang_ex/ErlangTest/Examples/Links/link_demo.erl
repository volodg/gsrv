-module(link_demo).
-export([start/0, demo/0, demonstrate_normal/0, demonstrate_exit/1,
	demonstrate_error/0, demonstrate_message/1]).

start() ->
	register(demo, spawn(link_demo, demo, [])).

demo() ->
	process_flag(trap_exit, true),
	demo1().

demo1() ->
	receive
		{'EXIT', From, normal} ->
			io:format(
				"Demo process received normal exit from ~w~n",
			                [From]),
			demo1();
		{'EXIT', From, Reason} ->
			io:format(
				"Demo process received exit signal ~w from ~w~n",
				[Reason, From]),
			demo1();
		finished_demo ->
			io:format("Demo finished ~n", []);
		Other ->
			io:format("Demo process message ~w~n", [Other]),
			demo1()
	end.

demonstrate_normal() ->
	link(whereis(demo)).

demonstrate_exit(What) ->
	link(whereis(demo)),
	exit(What).

demonstrate_message(What) ->
	demo ! What.

demonstrate_error() ->
	link(whereis(demo)),
	1 = 2.