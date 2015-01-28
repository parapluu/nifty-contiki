-module(nifty_cooja_recorder).

%% recorder
-export([record_event/2,
	 retrieve_events/1,
	 start_recorder/0,
	 stop_recorder/1,
	 events_to_calls/1]).

-export([make_simfile_static/3]).

record_event({_,Opts}, Event) ->
    case proplists:get_value(record, Opts) of
	undefined ->
	    erlang:error(corrupt_handler);
	Recorder ->
	    Recorder ! {self(), {record, Event}},
	    ok = receive
		     ok ->
			 ok
		 end,
	    ok
    end.

retrieve_events({_, Opts}) ->
    case proplists:get_value(record, Opts) of
	undefined ->
	    erlang:error(corrupt_handler);
	Recorder ->
	    Recorder ! {self(), retrieve},
	    receive
		     Events ->
			 Events
	    end
    end.

events_to_calls(Events) ->
    events_to_calls(Events, []).

events_to_calls([], Acc) -> Acc;
events_to_calls([A,C|Tail], Acc) ->
    {answer, Time, RetVal} = A,
    {Call, Args} = case C of
		       {call, {_, RCall, RArgs}} -> {RCall, RArgs};
		       {call, {_, RCall}} -> {RCall, {}}
		   end,
    events_to_calls(Tail, [{Time, Call, Args, RetVal}|Acc]).

start_recorder() ->
    spawn(fun() -> recorder_loop([]) end).

recorder_loop(List) ->
    receive
	{Pid, retrieve} ->
	    Pid ! List,
	    recorder_loop([]);
	{Pid, {record, Event}} ->
	    Pid ! ok,
	    recorder_loop([Event|List]);
	{Pid, stop} ->
	    Pid ! ok
    end.

stop_recorder({_, Opts}) ->
    case proplists:get_value(record, Opts) of
	undefined ->
	    ok; %% ignor instances with no recorders
	Recorder ->
	    Recorder ! {self(), stop},
	    ok = receive
		     ok -> ok
		 end
    end.

%% ----------------------------------------------------------------
%% Simfile Output

make_simfile_static(File, Events, Outfile) ->
    Calls = events_to_calls(Events),
    {InitCalls, RuntimeCalls} = get_init_calls(Calls),
    OriginalRoot = nifty_xmlhelper:parse_xml(File),
    InitializedRoot = build_initialization(OriginalRoot, InitCalls),
    Script = build_runtimscript(RuntimeCalls),
    StaticRoot = nifty_xmlhelper:add_script(InitializedRoot, Script),
    FinalRoot = nifty_xmlhelper:remove_controler(StaticRoot),
    nifty_xmlhelper:export_xml(FinalRoot, Outfile).

get_init_calls(Calls) ->
    {L, R} = lists:partition(fun ({Time, _, _, _}) -> Time=:=0 end, Calls),
    io:format("Partition: ~p, ~p~n", [length(L), length(R)]),
    {L, R}.

build_initialization(Root, Calls) ->
    Motes = build_motes(Root, Calls),
    io:format("Motes: ~n~p~n", [Motes]),
    lists:foldl(fun (Mote, D) -> nifty_xmlhelper:add_mote(D, Mote) end, Root, Motes).

build_motes(D, Calls) ->
    build_motes(D, Calls, dict:new()).

build_motes(_, [], Acc) ->
    lists:map(fun ({_, V}) -> V end, dict:to_list(Acc));
build_motes(D, [C|T], Acc) ->
    NewAcc = case C of
		 {_, mote_add, {Type}, {ok, MoteId}} ->
		     MoteNoId = nifty_xmlhelper:new_mote(Type),
		     Mote = nifty_xmlhelper:mote_set_id(D, MoteNoId, MoteId),
		     dict:store(MoteId, Mote, Acc);
		 {_, mote_set_pos, {MoteId, X, Y, Z}, _} ->
		     MoteNoPos = dict:fetch(MoteId, Acc),
		     Mote = nifty_xmlhelper:mote_set_position(D, MoteNoPos, {X,Y,Z}),
		     dict:store(MoteId, Mote, Acc);		     
		 _ ->
		     io:format("Call: ~p~n", [C]),
		     Acc
	     end,
    build_motes(D, T, NewAcc).

build_runtimscript(_RuntimeCalls) ->
    "".
