% to use:		$ erl
% to compile:	> c(module). [module = filename = erlang]
% to run:		> module:function(args).
-module(erlang).
-compile(export_all).

% Factorial of X
factorial(N) when N > 0 -> N * factorial(N-1);
factorial(0) -> 1.

% Converts cm <--> in
convert({cm, X}) -> {in, X/2.54};
convert({in, X}) -> {cm, X*2.54}.

% Length of a list
listLength([_ | Tail]) -> 1 + listLength(Tail);
listLength([]) -> 0.

% Number of days for a month in a year
daysInMonth(Year, Month) ->
	Leap =
		if
			trunc(Year/400) * 400 == Year -> leap;
			trunc(Year/100) * 100 == Year -> not_leap;
			trunc(Year/4) * 4 == Year -> leap;
			true -> not_leap
		end,

	case Month of
		jan -> 31;
		feb when Leap == leap -> 29;
		feb -> 28;
		mar -> 31;
		apr -> 30;
		may -> 31;
		jun -> 30;
		jul -> 31;
		aug -> 31;
		sep -> 30;
		oct -> 31;
		nov -> 30;
		dec -> 31
	end.

% Same as the map function
% erlang:mapFunc(fun(X) -> X*X end, [1,2,3]). ​​=> [1,4,9]
mapFunc(Func, [X | Rest]) -> [Func(X) | mapFunc(Func, Rest)];
mapFunc(_, []) -> [].


% Concurrent programming

% ~p = standard format, ~n = \n

% Prints the messages "hey" and "bye" three times each, concurrently, with the start() function
saySomething(_, 0) -> done;
saySomething(What, XTimes) ->
	io:format("~p~n", [What]),
	saySomething(What, XTimes-1).

start() ->
	spawn(erl, saySomething, [hey, 3]),
	spawn(erl, saySomething, [bye, 3]).

% Process that calculates the area of a circle or a rectangle, when getting the message "exit" the process ends
areas() ->
	receive
		{circle, Radio} ->
			io:format("Area = ~p~n", [math:pi() * Radio * Radio]),
			areas();
		{rectangle, Ancho, Alto} ->
			io:format("Area = ~p~n", [Ancho * Alto]),
			areas();
		exit -> bye;
		Else ->
			io:format("Error area ~p~n", [Else]),
			areas()
	end.

% Process that accepts a message {hello, P}, where P is assumed to be a Pid. For each message, the process sends {reply, C} to P (where C is a count.)
hello() ->
	receive
		{hello, P} -> P ! {reply, 0}, hello(1)
	end.

hello(Count) ->
	receive
		{hello, P} -> P ! {reply, Count}, hello(Count+1)
	end.
% Test program for the hello process
testHello() ->
	H = spawn(erlang, hello, []),
	testHello(10, H).
testHello(N, H) when N > 0 ->
	H ! {hello, self()},
	receive
		{reply, C} ->
			io:format("Received ~w~n", [C]),
			testHello(N-1, H)
	end;
test_hello(_, _) -> io:format("My work is done~n").