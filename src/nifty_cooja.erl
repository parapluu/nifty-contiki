-module(nifty_cooja).
-export([%% cooja
	 start/2,
	 start/3,
	 state/0,
	 exit/0,
	 quit_cooja/1,
	 %% simulation
	 start_simulation/1,
	 stop_simulation/1,
	 is_running/1,
	 set_speed_limit/2,
	 set_random_seed/2,
	 simulation_time/1,
	 simulation_time_ms/1,
	 simulation_step_ms/1,
	 simulation_step/2,
	 %% motes
	 motes/1,
	 mote_write/3,
	 mote_listen/2,
	 mote_unlisten/2,
	 mote_hw_listen/2,
	 mote_hw_unlisten/2,
	 mote_hw_events/2,
	 mote_read_pushback/3,
	 mote_read/2,
	 mote_read_s/2,
	 msg_wait/2,
	 get_last_event/2,
	 %% nifty interface
	 alloc/3,
	 alloc/4,
	 free/3,
	 write/3,
	 write/4,
	 read/4,
	 %% higher level
	 wait_for_result/2,
	 wait_for_result/3,
	 wait_for_msg/4,
	 next_event/2
	]).

start_node() ->
    [] = os:cmd("epmd -daemon"),
    case net_kernel:start([cooja_master, shortnames]) of
	{ok, _} ->
	    ok;
	{error, {already_started, _}} ->
	    ok
    end,
    case lists:member(cooja_master, registered()) of
	true ->
	    fail;
	false ->
	    register(cooja_master, self()),
	    ok
    end.

start(CoojaPath, Simfile) ->
    start(CoojaPath, Simfile, []).

start(CoojaPath, Simfile, Options) ->
    {ok, OldPath} = file:get_cwd(),
    Return = case start_node() of
		 ok ->
		     case lists:member(cooja_server, registered()) of
			 true -> 
			     fail;
			 false ->
			     CmdTmpl = case lists:member(gui, Options) of
					   true ->
					       "java -jar ~s -quickstart=~s";
					   _ ->
					       "java -jar ~s -nogui=~s"
				       end,
			     AbsSimPath = filename:absname(Simfile),
			     Cmd = format(CmdTmpl, ["cooja.jar", AbsSimPath]),
			     P = spawn(fun () -> start_command(CoojaPath, Cmd, lists:member(debug, Options)) 
				       end),
			     true = register(cooja_server, P),
			     receive
				 {pid, Pid} ->
				     P ! {handler, Pid},
				     Pid
			     end
		     end;
		 fail ->
		     fail
	     end,
    ok = file:set_cwd(OldPath),
    Return.

wait_for_cooja() ->
    case state() of
	{running, _} ->
	    timer:sleep(100),
	    wait_for_cooja();
	Exit ->
	    Exit
    end.

exit() ->
    case state() of
	{running, Handler} ->
	    ok = quit_cooja(Handler),
	    wait_for_cooja();
	E ->
	    E
    end.

state() ->
    case lists:member(cooja_server, registered()) of
	true ->
	    P = whereis(cooja_server),
	    P ! {state, self()},
	    receive
		{finished, {0, _}} -> ok;
		{finished, {R, O}} -> {error, {R, O}};
		{running, Handler} -> {running, Handler}
	    end;
	false->
	    not_running
    end.

start_command(CoojaPath, Cmd, Debug) ->
    P = spawn(fun () ->command_fun(CoojaPath, Cmd, Debug) end),
    P ! {handler, self()},
    Handler = receive
		  {handler, H} -> H
	      end,
    handle_requests(Handler).

handle_requests(Handler) ->
    receive
	{result, {R, O}} ->
	    receive
		{state, P} ->
		    P ! {finished, {R, O}}
	    end;
	{state, P} ->
	    P ! {running, Handler},
	    handle_requests(Handler)
    end.

command_fun(CoojaPath, Cmd, PrintOutput) ->
    S = receive
	    {handler, P} -> P
	end,
    {ok, OldPath} = file:get_cwd(),
    ok = file:set_cwd(filename:join([CoojaPath, "dist"])),
    {R, O} = command(Cmd),
    ok = case PrintOutput of
	     true -> io:format("~s~n", [lists:flatten(O)]);
	     false -> ok
	 end,
    ok = file:set_cwd(OldPath),
    true = case lists:member(cooja_master, registered()) of
	       true ->
		   unregister(cooja_master);
	       _ ->
		   true
	   end,
    case PrintOutput of
	true ->
	    S ! {result, {R, O}};
	false ->
	    S ! {result, {R, "set debug option to see the output"}}
    end.

format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

%% Taken form EUnit
%% ---------------------------------------------------------------------
%% Replacement for os:cmd

%% TODO: Better cmd support, especially on Windows (not much tested)
%% TODO: Can we capture stderr separately somehow?

command(Cmd) ->
    command(Cmd, "").

command(Cmd, Dir) ->
    command(Cmd, Dir, []).

command(Cmd, Dir, Env) ->
    CD = if Dir =:= "" -> [];
	    true -> [{cd, Dir}]
	 end,
    SetEnv = if Env =:= [] -> []; 
		true -> [{env, Env}]
	     end,
    Opt = CD ++ SetEnv ++ [stream, exit_status, use_stdio,
			   stderr_to_stdout, in, eof],
    P = open_port({spawn, Cmd}, Opt),
    get_data(P, []).

get_data(P, D) ->
    receive
	{P, {data, D1}} ->
	    get_data(P, [D1|D]);
	{P, eof} ->
	    port_close(P),    
	    receive
		{P, {exit_status, N}} ->
		    {N, normalize(lists:flatten(lists:reverse(D)))}
	    end
    end.

normalize([$\r, $\n | Cs]) ->
    [$\n | normalize(Cs)];
normalize([$\r | Cs]) ->
    [$\n | normalize(Cs)];
normalize([C | Cs]) ->
    [C | normalize(Cs)];
normalize([]) ->
    [].

%% ---------------------------------------------------------------------
%% Commands

quit_cooja(Handler) ->
    Handler ! {self(), quit_cooja},
    receive
	ok -> ok
    end.    

start_simulation(Handler) ->
    Handler ! {self(), start_simulation},
    receive
	ok -> ok
    end.

stop_simulation(Handler) ->
    Handler ! {self(), stop_simulation},
    receive
	ok -> ok
    end.

set_speed_limit(Handler, SpeedLimit) ->
    Handler ! {self(), set_speed_limit, {SpeedLimit}},
    receive
	ok -> ok
    end.

set_random_seed(Handler, Seed) ->
    Handler ! {self(), set_random_seed, {Seed}},
    receive
	ok -> ok
    end.


is_running(Handler) ->
    Handler ! {self(), is_running},
    receive
	State -> State
    end.

simulation_time(Handler) ->
    Handler ! {self(), simulation_time},
    receive
	Time -> Time
    end.

simulation_time_ms(Handler) ->
    Handler ! {self(), simulation_time_ms},
    receive
	Time -> Time
    end.

simulation_step_ms(Handler) ->
    Handler ! {self(), simulation_step_ms},
    receive
	Rsp -> Rsp
    end.

simulation_step(Handler, Time) ->
    Handler ! {self(), simulation_step, {Time}},
    receive
	Rsp -> Rsp
    end.

motes(Handler) ->
    Handler ! {self(), motes},
    receive
	Motes -> Motes
    end.

mote_write(Handler, Mote, Data) ->
    Handler ! {self(), mote_write, {Mote, Data}},
    receive
	ok -> ok
    end.

mote_listen(Handler, Mote) ->
    Handler ! {self(), mote_listen, {Mote}},
    receive
	ok -> ok
    end.

mote_unlisten(Handler, Mote) ->
    Handler ! {self(), mote_unliste, {Mote}},
    receive
	ok -> ok
    end.

mote_hw_listen(Handler, Mote) ->
    Handler ! {self(), mote_listen, {Mote}},
    receive
	ok -> ok
    end.

mote_hw_unlisten(Handler, Mote) ->
    Handler ! {self(), mote_unliste, {Mote}},
    receive
	ok -> ok
    end.

mote_hw_events(Handler, Mote) ->
    Handler ! {self(), mote_hw_events, {Mote}},
    receive
	Rsp -> Rsp
    end.

mote_read_pushback(Handler, Mote, Data) ->
    Handler ! {self(), mote_read_pushback, {Mote, Data}},
    receive
	ok -> ok
    end.

mote_read(Handler, Mote) ->
    Handler ! {self(), mote_read, {Mote}},
    receive
	Data -> 
	    Data
    end.

mote_read_s(Handler, Mote) ->
    Handler ! {self(), mote_read_s, {Mote}},
    receive
	Data ->
	    Data
    end.

msg_wait(Handler, Msg) ->
    Handler ! {self(), msg_wait, {Msg}},
    receive
	ok -> ok
    end.

get_last_event(Handler, Id) ->
    Handler ! {self(), get_last_event, {Id}},
    receive
	no_event -> no_event;
	R -> string:substr(R, 7, length(R)-7)
    end.

%% high-level stuff
next_event(Handler, Mote) ->
    case get_last_event(Handler, Mote) of
	not_listened_to ->
	    fail;
	badid ->
	    badid;
	updated ->
	    next_event(Handler, Mote);
	no_event ->
	    ok = simulation_step_ms(Handler),
	    next_event(Handler, Mote);
	E ->
	    E
    end.

wait_for_result(Handler, Mote) ->
    wait_for_result(Handler, Mote, 1000).

wait_for_result(Handler, Mote, T) ->
    wait_for_result(Handler, Mote, T, "").

wait_for_result(_,_,-1,_) -> throw(timeout);
wait_for_result(Handler, Mote, T, Acc) ->
    case state() of
	{running, _} ->
	    S = mote_read(Handler, Mote),
	    RAW_NS = Acc++S,
	    %% get rid of events
	    NS = re:replace(RAW_NS, "EVENT:[^\n]*\n", "", [{return, list}, global]),
	    case re:run(NS, "DEBUG[^\n]*\n") of
		{match, [{Start,Length}]} ->
		    ok = simulation_step_ms(Handler),
		    io:format("<<~p>>~n", [string:substr(NS, Start+1, Length)]),
		    RS = re:replace(NS, "DEBUG[^\n]*\n", "", [{return, list}]),
		    wait_for_result(Handler, Mote, T-1, RS);
		_ ->
		    case re:run(NS, ".*\n") of
			{match, [{Start, Length}]} ->
			    %% get one message
			    CRCS = string:substr(NS, Start+1, Length),
			    PB = string:substr(NS, Start+Length+1),
			    case PB of
				[] -> ok;
				_ ->
				    %% push back the remaining message
				    io:format("Pushback ~p of ~p ~n", [PB, NS]), 
				    mote_read_pushback(Handler, Mote, string:substr(NS, Start+Length+1))
			    end,
			    CRCS;
			nomatch ->
			    ok = simulation_step_ms(Handler),
			    wait_for_result(Handler, Mote, T-1, NS)
		    end
	    end;
	_ -> 
	    undef
    end.

wait_for_msg(Handler, Receiver, Timeout, Msg) ->
    wait_for_msg(Handler, Receiver, Timeout*500, Msg, "").

wait_for_msg(_, _, -1, _, _) -> throw(timeout);
wait_for_msg(Handler, Receiver, T, Msg, Acc) ->
    case state() of
	{running, _} ->
	    S = mote_read(Handler, Receiver),
	    RAW_NS = Acc++S,
	    %% get rid of events
	    NS = re:replace(RAW_NS, "EVENT:[^\n]*\n", "", [{return, list}, global]),
	    case re:run(NS, "DEBUG[^\n]*\n") of
		{match, [{_,_}]} ->
		    %% io:format("~s", [string:substr(NS, Start+1, Length)]),
		    ok = simulation_step_ms(Handler),
		    RS = re:replace(NS, "DEBUG[^\n]*\n", "", [{return, list}]),
		    wait_for_msg(Handler, Receiver, T-1, Msg, RS);
		_ ->
		    case re:run(NS, Msg) of
			{match, _} ->
			    true;
			nomatch ->
			    ok = simulation_step_ms(Handler),
			    wait_for_msg(Handler, Receiver, T-1, Msg, NS)
		    end
	    end;
	_ ->
	    false
    end.

stop_cond(Handler) ->
    case is_running(Handler) of
	true ->
	    stop_simulation(Handler),
	    true;
	false ->
	    false
    end.

start_cond(Handler, St) ->
    case St of
	true ->
	    start_simulation(Handler);
	false ->
	    ok
    end.

%% nifty functions
alloc(Handler, Mote, Size) ->
    alloc(Handler, Mote, Size, 1000).

alloc(Handler, Mote, Size, Wait) ->
    St = stop_cond(Handler),
    Command = format("-2 ~.b~n",[Size]),
    mote_write(Handler, Mote, Command),
    Result = case Wait of 
		 0 ->
		     ok;
		 T ->
		     R = wait_for_result(Handler, Mote, T),
		     %% 0x1234\n -> cut of first two and last character
		     %% io:format("Alloc result: <<~p>> from command: <<~p>>", [R, Command]),
		     list_to_integer(string:substr(R, 3, length(R)-3), 16)
	     end,
    ok = start_cond(Handler, St),
    Result.

free(Handler, Mote, Size) ->
    free(Handler, Mote, Size, 1000).

free(Handler, Mote, Size, Wait) ->
    St = stop_cond(Handler),
    Command = format("-5 ~.16b~n",[Size]),
    mote_write(Handler, Mote, Command),
    Result = case Wait of 
		 0 ->
		     ok;
		 T ->
		     true = wait_for_msg(Handler, Mote, T, "ok\n"),
		     ok
	     end,
    ok = start_cond(Handler, St),
    Result.

write(Handler, Mote, Data) ->
    Ptr = alloc(Handler, Mote, length(Data)),
    ok = write(Handler, Mote, Ptr, Data),
    Ptr.

write(Handler, Mote, Ptr, Data) ->
    St = stop_cond(Handler),
    Result = write_chunks(Handler, Mote, Data, Ptr, 20),
    ok = start_cond(Handler, St),
    Result.

write_chunks(_, _, [], _, _) -> ok;
write_chunks(Handler, Mote, Data, Ptr, ChS) -> 
    ToWrite = lists:sublist(Data, ChS),
    Rest = lists:nthtail(length(ToWrite), Data),
    CommandData = lists:flatten([format("~2.16.0b", [X]) || X<-ToWrite]),
    Command = format("-1 ~.16b ~s~n", [Ptr, CommandData]),
    mote_write(Handler, Mote, Command),
    case wait_for_result(Handler, Mote)=:="ok\n" of
    	true ->
    	    write_chunks(Handler, Mote, Rest, Ptr+length(ToWrite), ChS);
    	_ ->
    	    undef
    end.

read(Handler, Mote, Ptr, Size) ->
    St = stop_cond(Handler),
    Result = read_chunks(Handler, Mote, Ptr, Size, 20),
    ok = start_cond(Handler, St),
    Result.

read_chunks(Handler, Mote, Ptr, Size, ChS) ->
    read_chunks(Handler, Mote, Ptr, Size, ChS, []).

read_chunks(_, _, _, 0, _, Data) -> Data;
read_chunks(Handler, Mote, Ptr, Size, ChS, Acc) -> 
    ReadSize = case Size<ChS of
		   true ->
		       Size;
		   _ ->
		       ChS
	       end,
    Command = format("-3 ~.16b ~.b~n", [Ptr, ReadSize]),
    mote_write(Handler, Mote, Command),
    case wait_for_result(Handler, Mote) of
	undef ->
	    undef;
	RawData ->
	    read_chunks(Handler, Mote, Ptr+ReadSize, Size-ReadSize, ChS, Acc ++ pairs(lists:droplast(RawData)))
    end.

pairs(L) ->
    [list_to_integer(I, 16) ||I <-string:tokens(L, ",")].
