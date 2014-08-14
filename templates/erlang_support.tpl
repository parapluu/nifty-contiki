-module({{module}}).
-export([{% with fn=symbols|fetch_keys %}{% for name in fn %}
	'{{name}}'/{{ symbols|fetch:name|length|add:1 }},{% endfor %}{% endwith %}
	get_types/0
	]).

-define(TYPES, {{types}}).

get_types() -> ?TYPES.

format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

{% with fn=symbols|fetch_keys %}{% for name in fn %}'{{name}}'(Handler, Mote{% with arguments=symbols|fetch:name %}{% for argument in arguments %}{% if argument|is_argument %},A{{argument|getNth:2}}{% endif %}{% endfor %}{% endwith %}) ->
   F = [
{% with arguments=symbols|fetch:name %}{% for argument in arguments %}{% if argument|is_argument %}{% with raw_type=argument|getNth:3 phase="erlformat" N=argument|getNth:2 %}{% with type=raw_type|resolved:types carg="carg_"|add:N %}{% include "lib/builtin_type.tpl" %}{% endwith %}{% endwith %}{% if not forloop.last %},{%endif%}
{% endif %}{% endfor %}{% endwith %}
       ],

   C = format("~p " ++ lists:flatten(F) ++ "~n", [
   {{forloop.counter0}}
{% with arguments=symbols|fetch:name %}
   {% for argument in arguments %}
      {% if argument|is_argument %}
      	 ,A{{argument|getNth:2}}
      {% endif %}
   {% endfor %}
{% endwith %}
   ]),
   nifty_cooja:mote_write(Handler, Mote, C),
   R = nifty_cooja:wait_for_result(Handler, Mote, 1000),

  {% with arguments=symbols|fetch:name %}
    {% for argument in arguments %}
      {% if argument|is_return %}
        {% with raw_type=argument|getNth:2 phase="erlconvert" N=argument|getNth:2 %}
          {% with type=raw_type|resolved:types carg="c_retval" left=maxbuf buffer="nifty_buffer" %}
            {% include "lib/builtin_type.tpl" %}
          {% endwith %}
        {% endwith %}
      {% endif %}
    {% endfor %}
  {% endwith %}
   .

{% endfor %}{% endwith %}
