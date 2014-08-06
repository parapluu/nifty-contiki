-module(nifty_cooja).
-export([start/2,
	 start/3,
	 state/0,
	 quit_cooja/1,
	 start_simulation/1,
	 stop_simulation/1,
	 is_running/1,
	 set_speed_limit/2,
	 simulation_time/1,
	 simulation_time_ms/1,
	 simulation_step_ms/1,
	 motes/1,
	 mote_write/3,
	 mote_listen/2,
	 mote_unlisten/2,
	 mote_read/2,
	 mote_read_s/2,
	 msg_wait/2
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
    start(CoojaPath, Simfile, false).

start(CoojaPath, Simfile, Debug) ->
    case start_node() of
	ok ->
	    case lists:member(cooja_server, registered()) of
		true -> 
		    fail;
		false ->
		    CmdTmpl = "java -jar ~s -nogui=~s",
		    AbsSimPath = filename:absname(Simfile),
		    Cmd = format(CmdTmpl, ["cooja.jar", AbsSimPath]),
		    P = spawn(fun () -> start_command(CoojaPath, Cmd, Debug) 
			      end),
		    true = register(cooja_server, P),
		    ok
	    end,
	    receive
	    	{pid, Pid} ->
	    	    Pid
	    end;
	fail ->
	    fail
    end.

state() ->
    case lists:member(cooja_server, registered()) of
	true ->
	    P = whereis(cooja_server),
	    P ! {state, self()},
	    receive
		{finished, {0, _}} -> ok;
		{finished, {R, O}} -> {error, {R, O}};
		running -> running
	    end;
	false->
	    not_running
    end.

start_command(CoojaPath, Cmd, Debug) ->
    P = spawn(fun () ->command_fun(CoojaPath, Cmd, Debug) end),
    P ! {handler, self()},
    handle_requests().

handle_requests() ->
    receive
	{result, {R, O}} ->
	    receive
		{state, P} ->
		    P ! {finished, {R, O}}
	    end;
	{state, P} ->
	    P ! running,
	    handle_requests()
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
    unregister(cooja_master),
    S ! {result, {R, O}}.

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

mote_read(Handler, Mote) ->
    Handler ! {self(), mote_read, {Mote}},
    receive
	Data -> Data
    end.

mote_read_s(Handler, Mote) ->
    Handler ! {self(), mote_read_s, {Mote}},
    receive
	Data -> Data
    end.

msg_wait(Handler, Msg) ->
    Handler ! {self(), msg_wait, {Msg}},
    receive
	ok -> ok
    end.
