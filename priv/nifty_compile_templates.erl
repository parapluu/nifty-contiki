-module(nifty_compile_templates).
-export([main/1]).

main(_) ->
    ok = io:format("Compiling Templates~n"),
    Options = [{out_dir, "ebin"}, 
	       {force_recompile, true}, 
	       {custom_tags_modules, [nifty_tags]}, 
	       {custom_filters_modules, [nifty_filters]}, 
	       {compiler_options, [debug_info]},
	       return_errors],
    case erlydtl:compile("templates/contiki.tpl", nifty_contiki_template, Options) of 
	{ok, nifty_contiki_template} ->
	    ok;
	E ->
	    io:format("Error: ~p~n", [E])
    end.



