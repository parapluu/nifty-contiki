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
    ok = case erlydtl:compile("templates/contiki.tpl", nifty_contiki_template, Options) of 
	     {ok, nifty_contiki_template} ->
		 ok;
	     E1 ->
		 io:format("Error: ~p~n", [E1])
	 end,
    ok = case erlydtl:compile("templates/erlang_support.tpl", nifty_support_template, Options) of 
	     {ok, nifty_support_template} ->
		 ok;
	     E2 ->
		 io:format("Error: ~p~n", [E2])
	 end.
