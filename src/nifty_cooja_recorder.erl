%%% -------------------------------------------------------------------
%%% Copyright (c) 2015, Andreas Löscher <andreas.loscher@it.uu.se> and
%%%                     Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-module(nifty_cooja_recorder).

%% recorder
-export([record_event/2,
	 retrieve_events/1,
	 start_recorder/0,
	 stop_recorder/1,
	 events_to_calls/1]).

-export([make_simfile_static/3, remove_empty_lines/1, strip/1]).

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
    events_to_calls(lists:reverse(Events), [], 0).

events_to_calls([], Acc, _) -> lists:reverse(Acc);
events_to_calls([C,A|Tail], Acc, LastTime) ->
    {answer, Time, RetVal} = A,
    {Call, Args} = case C of
		       {call, {_, RCall, RArgs}} -> {RCall, RArgs};
		       {call, {_, RCall}} -> {RCall, {}}
		   end,
    events_to_calls(Tail, [{Time, (Time-LastTime) div 1000, Call, Args, RetVal}|Acc], Time).

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
    Motes = nifty_xmlhelper:extrace_motes(InitializedRoot),
    Script = build_runtimscript(RuntimeCalls, Motes),
    StaticRoot = nifty_xmlhelper:add_script(InitializedRoot, Script),
    FinalRoot = nifty_xmlhelper:remove_controler(StaticRoot),
    nifty_xmlhelper:export_xml(FinalRoot, Outfile).

get_init_calls(Calls) ->
    {L, R} = lists:partition(fun ({Time, _, _, _, _}) -> Time=:=0 end, Calls),
    {L, R}.

build_initialization(Root, Calls) ->
    Motes = build_motes(Root, Calls),
    lists:foldl(fun (Mote, D) -> nifty_xmlhelper:add_mote(D, Mote) end, Root, Motes).

build_motes(D, Calls) ->
    build_motes(D, Calls, dict:new()).

build_motes(_, [], Acc) ->
    lists:map(fun ({_, V}) -> V end, dict:to_list(Acc));
build_motes(D, [C|T], Acc) ->
    NewAcc = case C of
			   {_, _, mote_add, {Type}, {ok, MoteId}} ->
			       MoteNoId = nifty_xmlhelper:new_mote(Type),
			       Mote = nifty_xmlhelper:mote_set_id(D, MoteNoId, MoteId),
			       dict:store(MoteId, Mote, Acc);
			   {_, _, mote_set_pos, {MoteId, X, Y, Z}, _} ->
			       MoteNoPos = dict:fetch(MoteId, Acc),
			       Mote = nifty_xmlhelper:mote_set_position(D, MoteNoPos, {X,Y,Z}),
			       dict:store(MoteId, Mote, Acc);
			   _ ->
			       %% io:format("Call: ~p~n", [C]),
			       Acc
		       end,
    build_motes(D, T, NewAcc).    

build_runtimscript(RuntimeCalls, Motes) ->
    RenderVars = [{"init_motes", Motes},
		  {"calls", RuntimeCalls}],
    ScriptOut = lists:flatten(nifty_compiler:render_with_errors(nifty_simscript_template, RenderVars)),
    remove_empty_lines(ScriptOut).

remove_empty_lines(Str) ->
    lists:foldr(fun (L,R) -> L++"\n"++R end,
		"",
		lists:filter(fun (Val) -> Val=/=[] end, 
			     lists:map(fun strip/1, string:tokens(format("~s", [Str]), "\n")))).

strip(A) ->
    re:replace(A, "(^\\s+)|(\\s+$)", "", [global,{return,list}]).

format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).
