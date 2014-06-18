-module(nifty_cooja).
-export([start/2,
	 state/0,
	 connect/2 ,
	 disconnect/1,
	 call/3,
	 msg/2]).

connect(Host, Port) ->
    {ok, Conn} = gen_tcp:connect(Host, Port, [{active, false}]),
    Conn.

call(Conn, Func, Args) ->
    Format = string:copies("~p ", length(Args)) ++ "~p~n",
    Msg = io:fwrite(Format, [Func|Args]),
    gen_tcp:send(Conn, Msg).

msg(Conn, Msg) ->
    gen_tcp:send(Conn, io:fwrite("~p~n", [Msg])).

disconnect(Conn) ->
    gen_tcp:close(Conn).

start(CoojaPath, Simfile) ->
    Cmd = "java -jar ~p -nogui=~p",
    AbsSimPath = filename:absname(Simfile),
    _ = spawn(fun () -> command_fun(CoojaPath, 
				    io:fwrite(Cmd, ["cooja.jar", AbsSimPath]), 
				    self()) 
	      end),
    ok.

state() ->
    receive
	0 -> success;
	_ -> fail
    after
	0 -> running
    end.

command_fun(CoojaPath, Cmd, S) ->
    {ok, OldPath} = file:get_cwd(),
    ok = file:set_cwd(CoojaPath),
    {R, _} = command(Cmd),
    ok = file:set_cwd(OldPath),
    S ! R.

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
