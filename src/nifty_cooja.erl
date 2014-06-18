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
    Msg = format(Format, [Func|Args]),
    gen_tcp:send(Conn, Msg).

msg(Conn, Msg) ->
    gen_tcp:send(Conn, format("~s~n", [Msg])).

disconnect(Conn) ->
    gen_tcp:close(Conn).

start(CoojaPath, Simfile) ->
    case lists:member(cooja_server, registered()) of
	true -> 
	    fail;
	false ->
	    CmdTmpl = "java -jar ~s -nogui=~s",
	    AbsSimPath = filename:absname(Simfile),
	    Cmd = format(CmdTmpl, ["cooja.jar", AbsSimPath]),
	    P = spawn(fun () -> command_fun(CoojaPath, Cmd) 
		      end),
	    true = register(cooja_server, P),
	    ok
    end.

state() ->
    case lists:member(cooja_server, registered()) of
	true ->
	    P = whereis(cooja_server),
	    P ! {finnished, self()},
	    receive
		ok ->
		    P ! {get, self()},
		    R = receive
			    0 -> ok;
			    _ -> fail
			end,
		    P ! stop,
		    R
	    after
		100 -> running
	    end;
	false->
	    not_running
    end.

command_fun(CoojaPath, Cmd) ->
    {ok, OldPath} = file:get_cwd(),
    ok = file:set_cwd(filename:join([CoojaPath, "dist"])),
    {R, O} = command(Cmd),
    %% io:format("~s~n", [lists:flatten(O)]),
    ok = file:set_cwd(OldPath),
    command_finalize(R).

command_finalize(R) ->
    receive
	{finnished, S} ->
	    S ! ok,
	    command_finalize(R);
	{get, S} ->
	    S ! R;
	stop ->
	    ok
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
