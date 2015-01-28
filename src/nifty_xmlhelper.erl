-module(nifty_xmlhelper).
-export([parse_xml/1, export_xml/2]).
-export([new_mote/1, mote_set_position/3, mote_set_id/3, add_mote/2, add_script/2]).

-export([get_node/2, get_all_nodes/2, get_interface_key/3]).

parse_xml(FileName) ->
    case erlsom:simple_form_file(FileName, [{nameFun, fun nameFunc/3}]) of
	{ok, Element, _} -> Element;
	Err -> Err
    end.

export_xml(Root, FileName) ->
    Data = xmerl:export_simple([Root], xmerl_xml),
    file:write_file(FileName, Data).

nameFunc(Name,[],_) ->
    list_to_atom(Name);
nameFunc(Name, Namespace, _) ->
    list_to_atom("{" ++ Namespace ++ "}" ++ Name).

%% string helper
format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

strip(A) ->
    re:replace(A, "(^\\s+)|(\\s+$)", "", [global,{return,list}]).

%% xml tree and element operations for this simulation files
add_mote(Doc, Mote) ->
    {simconf, Args, Content} = Doc,
    {simulation, SimArg, SimC} = get_node(Content, simulation),
    NewSim = {simulation, SimArg, SimC ++ [Mote]},
    {simconf, Args, update_node(Content, simulation, NewSim)}.

add_script(Doc = {Name, Args, Content}, Script) -> 
    ScriptElement = {script, [], [Script]},
    PCElement = {plugin_config, [], [Script, 
				     {active, [], ["false"]}]},
    PluginElement = {plugin, [], [PCElement,
				  {width, [], ["100"]},
				  {z, [], ["0"]},
				  {height, [], ["100"]},
				  {location_x, [], ["20"]},
				  {location_y, [], ["20"]}]},
     {Name, Args, Content ++ [PluginElement]}.

new_mote(Type) ->
    {mote, [], [{breakpoints, [], []}, 
		new_motetype_identifier(Type)]}.

new_motetype_identifier(Type) ->
    {motetype_identifier, [], [Type]}.

mote_set_position(Doc, {mote, Args, Content}, {X,Y,Z}) ->
    MoteType = get_mote_type(Content),
    InterfaceKey = get_interface_key(Doc, MoteType, "Position"),
    PositionInterface = 
	{interface_config, 
	 [], 
	 [InterfaceKey,
	  {x, [], [format("~f", [X])]},
	  {y, [], [format("~f", [Y])]},
	  {z, [], [format("~f", [Z])]}]},
    {mote, Args, update_interfaces(Content, 
				   InterfaceKey,
				   PositionInterface)}.

mote_set_id(Doc, {mote, Args, Content}, Id) ->
    MoteType = get_mote_type(Content),
    InterfaceKey = get_interface_key(Doc, MoteType, "ID"),
    IdInterface = 
	{interface_config, 
	 [], 
	 [InterfaceKey,
	  {id, [], [format("~B", [Id])]}]},
    {mote, Args, update_interfaces(Content,
				   InterfaceKey,
				   IdInterface)}.

%% helper functions
update_interfaces(C, Key, Interf) ->
    update_interfaces(C, Key, Interf, []).

update_interfaces([], _, Interf, Acc) ->
    lists:reverse([Interf| Acc]);
update_interfaces([H = {interface_config, _, [Key|_]} | T], 
		  SearchKey, Interf, Acc) ->
    case strip(Key)=:=strip(SearchKey) of
	true ->
	    lists:reverse([Interf|Acc]) ++ T;
	_ ->
	    update_interfaces(T, Key, Interf, [H|Acc])
    end;
update_interfaces([H|T], Key, Interf, Acc) ->
    update_interfaces(T, Key, Interf, [H|Acc]).

get_interface_key(Doc, Type, Key) ->
    {simconf, _, SimContent} = Doc,
    {_, _, SimC} = get_node(SimContent, simulation),
    [{_, _, TypeC}] = lists:filter(
		   fun ({motetype, _, [_,{identifier, _, [SK]} | _]}) -> strip(SK)=:=Type;
		       (_) -> false end,
		   get_all_nodes(SimC, motetype)),
    InterfaceKeys = lists:map(fun ({_,_,[Val]}) -> Val end,
			get_all_nodes(TypeC, moteinterface)),
    [InterfaceKey] = lists:filter(fun (Class) -> string:str(strip(Class), Key)=/=0 end,
				  InterfaceKeys),
    strip(InterfaceKey).

get_node([], _) -> undefined;
get_node([Node = {Name, _, _} | _], Key) when Name =:= Key -> Node;
get_node([_|T], Key) -> get_node(T, Key).

get_mote_type(C) ->
    {_, _, [Type]} = get_node(C, motetype_identifier),
    Type.

get_all_nodes(C, K) ->
    get_all_nodes(C, K, []).

get_all_nodes([], _, Acc) -> Acc;
get_all_nodes([Node = {Name, _, _} | T], Key, Acc) when Name =:= Key -> 
    get_all_nodes(T, Key, [Node|Acc]);
get_all_nodes([_|T], Key, Acc) -> 
    get_all_nodes(T, Key, Acc).

update_node(C, Name, Value) ->
    update_node(C, Name, Value, []).

update_node([], _, Value, Acc) ->
    lists:reverse([Value|Acc]);
update_node([{K, _, _}|T], Name, Value, Acc) when K=:=Name->
    lists:reverse([Value|Acc]) ++ T;
update_node([H|T], Name, Value, Acc) ->
    update_node(T, Name, Value, [H|Acc]).
