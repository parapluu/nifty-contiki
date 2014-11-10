%%% -------------------------------------------------------------------
%%% Copyright (c) 2014, Andreas Löscher <andreas.loscher@it.uu.se> and
%%%                     Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

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
	 %% radio
	 radio_set_config/2,
	 radio_get_config/1,
	 radio_listen/1,
	 radio_unlisten/1,
	 radio_get_messages/1,
	 %% motes
	 mote_types/1,
	 mote_add/2,
	 mote_del/2,
	 mote_get_pos/2,
	 mote_set_pos/3,
	 motes/1,
	 mote_write/3,
	 mote_listen/2,
	 mote_unlisten/2,
	 mote_hw_listen/2,
	 mote_hw_unlisten/2,
	 mote_hw_events/2,
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
	 wait_for_msg/3,
	 wait_for_msg/4,
	 next_event/2,
	 next_event/3,
	 duty_cycle/2
	]).

-define(STEP_SIZE, 100).

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

start(RawCoojaPath, RawSimfile, Options) ->
    CoojaPath = nifty_utils:expand(RawCoojaPath),
    Simfile = nifty_utils:expand(RawSimfile),
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

radio_set_config(Handler, {Radio, Options}) ->
    Handler ! {self(), radio_set_config, {Radio, Options}},
    receive
	Rsp -> Rsp
    end.

radio_get_config(Handler) ->
    Handler ! {self(), radio_get_config},
    receive
	Rsp -> Rsp
    end.

radio_listen(Handler) ->
    Handler ! {self(), radio_listen},
    receive
	Rsp -> Rsp
    end.
    
radio_unlisten(Handler) ->
    Handler ! {self(), radio_unlisten},
    receive
	Rsp -> Rsp
    end.
    
radio_get_messages(Handler) ->
    Handler ! {self(), radio_get_messages},
    receive
	Rsp -> Rsp
    end.    

motes(Handler) ->
    Handler ! {self(), motes},
    receive
	Motes -> Motes
    end.

mote_types(Handler) ->
    Handler ! {self(), mote_types},
    receive
	Types -> Types
    end.

mote_add(Handler, Type) ->
    Handler ! {self(), mote_add, {Type}},
    receive
	Rsp -> Rsp
    end.    

mote_del(Handler, Id) ->
    Handler ! {self(), mote_del, {Id}},
    receive
	Rsp -> Rsp
    end.

mote_get_pos(Handler, Id) ->
    Handler ! {self(), mote_get_pos, {Id}},
    receive
	Pos -> Pos
    end.    

mote_set_pos(Handler, Id, Pos) ->
    {X, Y, Z} = Pos,
    Handler ! {self(), mote_set_pos, {Id, X, Y, Z}},
    receive
	Rsp -> Rsp
    end.

mote_write(Handler, Mote, Data) ->
    Handler ! {self(), mote_write, {Mote, Data}},
    receive
	ok -> ok
    end.

mote_listen(Handler, Mote) ->
    Handler ! {self(), mote_listen, {Mote}},
    receive
	Rsp -> Rsp
    end.

mote_unlisten(Handler, Mote) ->
    Handler ! {self(), mote_unliste, {Mote}},
    receive
	Rsp -> Rsp
    end.

mote_hw_listen(Handler, Mote) ->
    Handler ! {self(), mote_hw_listen, {Mote}},
    receive
	Rsp -> Rsp
    end.

mote_hw_unlisten(Handler, Mote) ->
    Handler ! {self(), mote_hw_unliste, {Mote}},
    receive
	Rsp -> Rsp
    end.

mote_hw_events(Handler, Mote) ->
    Handler ! {self(), mote_hw_events, {Mote}},
    receive
	Rsp -> Rsp
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
    next_event(Handler, Mote, 1000).

next_event(_, _, T) when T<0 -> throw(timeout);
next_event(Handler, Mote, T) ->
    case get_last_event(Handler, Mote) of
	not_listened_to ->
	    fail;
	badid ->
	    badid;
	no_event ->
	    ok = simulation_step(Handler, ?STEP_SIZE),
	    next_event(Handler, Mote, T-?STEP_SIZE);
	E ->
	    E
    end.

wait_for_result(Handler, Mote) ->
    wait_for_result(Handler, Mote, 1000).

wait_for_result(_,_,T) when T<0 -> throw(timeout);
wait_for_result(Handler, Mote, T) ->
    case state() of
	{running, _} ->
	    case mote_read(Handler, Mote) of
		"" ->
		    ok = simulation_step(Handler, ?STEP_SIZE),
		    wait_for_result(Handler, Mote, T-?STEP_SIZE);
		S ->
		    case re:run(S, "DEBUG[^\n]*\n") of
			{match, [{_,_}]} ->
			    ok = simulation_step(Handler, ?STEP_SIZE),
			    io:format("<<~p>>~n", [S]),
			    wait_for_result(Handler, Mote, T-?STEP_SIZE);
			_ ->
			    S
		    end
	    end;
	_ ->
	    undef
    end.

wait_for_msg(Handler, Mote, Msg) ->
    wait_for_msg(Handler, Mote, 1000, Msg).

wait_for_msg(_, _, T, _) when T<0 -> throw(timeout);
wait_for_msg(Handler, Mote, T, Msg) ->
    case state() of
	{running, _} ->
	    case mote_read(Handler, Mote) of
		"" ->
		    ok = simulation_step(Handler, ?STEP_SIZE),
		    wait_for_msg(Handler, Mote, T-?STEP_SIZE, Msg);
		S ->
		    case re:run(S, "DEBUG[^\n]*\n") of
			{match, [{_,_}]} ->
			    ok = simulation_step(Handler, ?STEP_SIZE),
			    io:format("<<~p>>~n", [S]),
			    wait_for_msg(Handler, Mote, T-?STEP_SIZE, Msg);
			_ ->
			    case re:run(S, Msg) of
				{match, _} ->
				    true;
				nomatch ->
				    ok = simulation_step(Handler, ?STEP_SIZE),
				    wait_for_msg(Handler, Mote, T-?STEP_SIZE, Msg)
			    end
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

duty_cycle(Events, SimTime) ->
    on_time(Events, 0, off, SimTime) / SimTime.

on_time([], Acc, off, _) -> Acc;
on_time([], Acc, Start, End) ->
    Acc + End - Start;
on_time([{radio,["on",Time]}|T], Acc, off, E) -> 
    on_time(T, Acc, erlang:list_to_integer(Time), E);
on_time([{radio,["off", _]}|T], Acc, off, E) -> 
    on_time(T, Acc, off, E);
on_time([{radio,["off", Time]}|T], Acc, Start, E) -> 
    on_time(T, Acc + erlang:list_to_integer(Time) - Start, off, E);
on_time([_|T], Acc, S, E) -> 
    on_time(T, Acc, S, E).
