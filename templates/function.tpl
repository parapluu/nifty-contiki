static char* BUFFER= "Return Value";

/* create call functions */

/*
* Variable transition inside of a function:
*	prepare -- are additional variable definitions required -> e.a. short int translation
*	to_c    -- translate the erlang values to c values
*	argument-- yield argument
*	to_erl  -- translate the return value and eventual output arguments to erlang types
*	cleanup -- cleanup memory used by the function
*/


static char* nifty_buffer[{{maxbuf}}];

{% with fn=symbols|fetch_keys %}
	{% for name in fn %}
int
niftycall_{{name}}(char* args) {
  unsigned forward;
  char* nextarg = args;
  char* curarg;

  {% with arguments=symbols|fetch:name %}
    {% for argument in arguments %}
      {% if argument|is_argument %}
        {% with raw_type=argument|getNth:3 phase="prepare" N=argument|getNth:2 %}
          {% with type=raw_type|resolved:types carg="carg_"|add:N %}
            {% include "lib/builtin_type.tpl" %}
          {% endwith %}
        {% endwith %}
      {% endif %}
      {% if argument|is_return %}
        {% with raw_type=argument|getNth:2 phase="prepare" N=argument|getNth:2 %}
          {% with type=raw_type|resolved:types carg="carg_"|add:N %}
            {% include "lib/builtin_type.tpl" %}
          {% endwith %}
        {% endwith %}
      {% endif %}
    {% endfor %}
  {% endwith %}

  {% with arguments=symbols|fetch:name %}
    {% for argument in arguments %}
      {% if argument|is_argument %}
        curarg = nextarg;
        {% with raw_type=argument|getNth:3 phase="to_c" N=argument|getNth:2 %}
          {% with type=raw_type|resolved:types carg="carg_"|add:N %}
            {% include "lib/builtin_type.tpl" %}
          {% endwith %}
        {% endwith %}
      {% endif %}
    {% endfor %}
  {% endwith %}
	
  {% with arguments=symbols|fetch:name %}
    {% for argument in arguments %}
      {% if argument|is_return %}
        {% with raw_type=argument|getNth:2 phase="argument" %}
          {% with type=raw_type|resolved:types %}
            {% if not type=="void" %}
  c_retval =
            {% endif %}
          {% endwith %}
        {% endwith %}
      {% endif %}
    {% endfor %}
  {% endwith %}

  {% with arguments=symbols|fetch:name %}
    {% for argument in arguments %}
      {% if argument|is_return %}
        {% with raw_type=argument|getNth:2 phase="argument" N=argument|getNth:2 %}
          {% with type=raw_type|resolved:types carg="carg_"|add:N %}
            {% include "lib/builtin_type.tpl" %}
          {% endwith %}
        {% endwith %}
      {% endif %}
    {% endfor %}
  {% endwith %}
	{{name}}(
  {% with arguments=symbols|fetch:name %}
    {% for argument in arguments %}
      {% if argument|is_argument %}
        {% with raw_type=argument|getNth:3 phase="argument" N=argument|getNth:2 %}
          {% with type=raw_type|resolved:types carg="carg_"|add:N %}
            {% include "lib/builtin_type.tpl" %}
          {% endwith %}
        {% endwith %}
      {% endif %}
      {% if not forloop.last and argument|is_argument %},{% endif %}
    {% endfor %}
  {% endwith %}
		);

  {% with arguments=symbols|fetch:name %}
    {% for argument in arguments %}
      {% if argument|is_return %}
        {% with raw_type=argument|getNth:2 phase="to_erl" N=argument|getNth:2 %}
          {% with type=raw_type|resolved:types carg="carg_"|add:N left=maxbuf buffer="nifty_buffer" %}
            {% include "lib/builtin_type.tpl" %}
          {% endwith %}
        {% endwith %}
      {% endif %}
    {% endfor %}
  {% endwith %}

  {% with arguments=symbols|fetch:name %}
    {% for argument in arguments %}
      {% if argument|is_argument %}
        {% with raw_type=argument|getNth:3 phase="cleanup" N=argument|getNth:2 %}
          {% with type=raw_type|resolved:types carg="c_retval" %}
            {% include "lib/builtin_type.tpl" %}
          {% endwith %}
        {% endwith %}
      {% endif %}
    {% endfor %}
  {% endwith %}

  return forward;
}
	{% endfor %}
{% endwith %}

void
process_input(char* input){
  unsigned function;
  unsigned outlength;
  int retval;
  char* arguments;
  function = strtol(input, &arguments, 10);
{% with fn=symbols|fetch_keys %}
  switch (function) {
  {% for name in fn %}
  case {{forloop.counter0}}:
    retval = niftycall_{{name}}(arguments);
    if (retval!=-1) {
      printf("%.*s\n", retval, nifty_buffer);
    } else {
      printf("badarg\n");
    }
    break;
    {% endfor %}
  }
{% endwith %}
}
