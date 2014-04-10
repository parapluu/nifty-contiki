-module(nifty_clangparse).
-export([parse/1, build_vars/1]).

-on_load(init/0).

init() -> %% loading code from jiffy
    PrivDir = case code:priv_dir(?MODULE) of
		  {error, _} ->
		      EbinDir = filename:dirname(code:which(?MODULE)),
		      AppPath = filename:dirname(EbinDir),
		      filename:join(AppPath, "priv");
		  Path ->
		      Path
	      end,
    erlang:load_nif(filename:join(PrivDir, "nifty_clangparse"), 0).

cparse(_) ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Takes clang compiler arguments, and returns a list of token and a proplist of functions names with 
%% their file locations
-spec parse([string()]) -> {{[string()],[{string(), string()}]}, Args} | {'fail', Args} when Args :: [string()].
parse(Args) ->
    {cparse(Args), Args}.

%% @doc Takes a list of token as produced by <code>parse/1</code> and returns type information about functions, structs
%% and typedefs
-spec build_vars([string()]) -> {dict:dict(), dict:dict(), dict:dict()}.
build_vars(Token) ->
    build_vars(Token, {dict:new(), dict:new(), dict:new()}).

build_vars([], Definitions) ->
    Definitions;
build_vars([H|T], Definitions) ->
    {NewT, Defs} = case H of
		       "FUNCTION" ->
			   build_function(T, Definitions);
		       "STRUCT" ->
			   build_struct(T, Definitions);
		       "TYPEDEF" ->
			   build_typedef(T, Definitions);
		       _ ->
			   io:format("r"),
			   recover(T, Definitions)
		       end,
    build_vars(NewT, Defs).

recover([], Defs) ->
    Defs;
recover([H|T], Defs) ->
    case H of
	"FUNCTION" ->
	    {[H|T], Defs};
	"STRUCT" ->
	    {[H|T], Defs};
	"TYPEDEF" ->
	    {[H|T], Defs};
	_ ->
	    recover(T, Defs)
    end.

build_type([Type|T], {Functions, TypeDefs, Structs}) ->
    {NT, Defs} = 
	case (string:str(Type, "<anonymous")>0) of
	    true-> 
		[_,_|RestToken] = T,
		build_named_struct(RestToken, {Functions, TypeDefs, Structs}, string:substr(Type, 8));
	    false ->
		{T, {Functions, TypeDefs, Structs}}
	end,
    {NT, Defs, Type}.

build_function([FuncName|T], {Functions, TypeDefs, Structs}) ->
    {PT, PD, Rettype} = build_rettype(T, {Functions, TypeDefs, Structs}),
    {NT, {FD, TD, SD}, Params} = build_params(PT, PD),
    NFD = dict:store(FuncName, {Rettype, Params}, FD),
    {NT, {NFD, TD, SD}}.

build_rettype([Type|T], Definitions) ->
    PureType = string:strip(string:substr(Type, 1, string:str(Type, "(")-1)),
    build_type([PureType|T], Definitions).

build_param([Ident|T], Definitions) ->
    case Ident of
	"PARAMETER" ->
	    [Name|TT] = T,
	    {NT, Defs, Type} = build_type(TT, Definitions),
	    {NT, Defs, {Name, Type}};
	_ -> {[Ident|T], Definitions, stop}
    end.

build_params(T, Definitions) ->
    {NT, Data, Params} = build_params(T, Definitions, []),
    {NT, Data, lists:reverse(Params)}.

build_params([], Definitions, Params) ->
    {[], Definitions, Params};
build_params(T, Definitions, Params) ->
    {NT, Defs, Param} = build_param(T, Definitions),
    case Param of
	stop -> {NT, Defs, Params};
	_ -> build_params(NT, Defs, [Param|Params])
    end.

build_field([Name|T], Definitions) ->
    {NT, Defs, Type} = build_type(T, Definitions),
    {NT, Defs, {Name, Type}}.

build_field_save([], Definitions, _) ->
    {[], Definitions, stop};
build_field_save([Ident|T], Definitions, ParentName) ->
    {_, TypeDefs, _} = Definitions,
    case Ident of
	"FIELD" -> 
	    [Parent|NT] = T,
	    case string:substr(resolve_type(strip_type_name(Parent), TypeDefs),8)=:=strip_type_name(ParentName) of
		true ->
		    build_field(NT, Definitions);
		false ->
		    {[Ident|T], Definitions, stop}
	    end;
	_ -> 
	    {[Ident|T], Definitions, stop}
    end.

build_fields(T, Definitions, Name) ->
    build_fields(T, Definitions, Name, []).

build_fields(T, Definitions, Name, Fields) ->
    {NT, Defs, Field} = build_field_save(T, Definitions, Name),
    case Field of
	stop -> 
	    {NT, Defs, Fields};
	_ -> 
	    build_fields(NT, Defs, Name, [Field|Fields])
    end.

build_named_struct(T, Definitions, Name) ->
    {NT, {Functions, TypeDefs, Structs}, Fields} = build_fields(T, Definitions, Name),
    {NT, {Functions, TypeDefs, dict:store(Name, Fields, Structs)}}.

build_anonymous_struct(T, Definitions) ->
    {T, Definitions}.

build_struct([Name|T], Definitions) ->
    case Name of
	[] ->
	    %% anonymous struct
	    build_anonymous_struct(T, Definitions);
	_ ->
	    build_named_struct(T, Definitions, Name)
    end.

build_typedef([Name| T], Definitions) ->
    {NT, {Functions, TypeDefs, Structs}, Type} = build_type(T, Definitions),
    {NT, {Functions, dict:store(Name, Type, TypeDefs), Structs}}.

strip_type_name(Name) ->
    Index = string:str(Name, "::"),
    Temp = case Index of
	       0 -> Name;
	       _ -> 
		   case string:str(Name, "struct") of
		       1 -> "struct " ++ string:substr(Name, Index+2);
		       _ -> string:substr(Name, Index+2)
		   end
	   end,
    case string:str(Temp, "struct") of
	12 -> string:substr(Temp, 1, 11) ++ string:substr(Temp, 19);
	_ -> Temp
    end.

resolve_type(Name, TypeDefs) -> 
    case dict:is_key(Name, TypeDefs) of
	true ->
	    [ResolvedName|_] = dict:fetch(Name, TypeDefs),
	    resolve_type(ResolvedName, TypeDefs);
	false ->
	    Name
    end.
