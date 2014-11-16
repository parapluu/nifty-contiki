/* create call functions */

/*
* Variable transition inside of a function:
*	prepare -- are additional variable definitions required -> e.a. short int translation
*	to_c    -- translate the erlang values to c values
*	argument-- yield argument
*	to_erl  -- translate the return value and eventual output arguments to erlang types
*	cleanup -- cleanup memory used by the function
*/

{% with fn=symbols|fetch_keys %}
	{% for name in fn %}
int
niftycall_{{name}}(char* args) {
  int forward;
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
          {% with type=raw_type|resolved:types carg="c_retval" left=maxbuf buffer="nifty_buffer" %}
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
          {% with type=raw_type|resolved:types carg="carg_"|add:N %}
            {% include "lib/builtin_type.tpl" %}
          {% endwith %}
        {% endwith %}
      {% endif %}
    {% endfor %}
  {% endwith %}

  return forward;
  curarg++;
  nextarg++;
}
	{% endfor %}
{% endwith %}

void
save_serial(char* buffer, int length) {
  if (length>100) {
    printf("fail\n");
  } else {
    printf("%.*s\n", length, buffer);
  }
}


void
process_input(char* input){
  long function;
  int retval;
  char* arguments;
  function = strtol(input, &arguments, 10);
  if ((!function) && (input==arguments)) {
    return; /* invalid input */
  }
  arguments++;
{% with fn=symbols|fetch_keys %}
 switch (function) {
 case -0x01:
   retval = write_mem(arguments);
   break;
 case -0x02:
   retval = allocate_mem(arguments);
   break;
 case -0x03:
   retval = read_mem(arguments);
   break;
 case -0x04:
   retval = nifty_sizeof(arguments);
   break;
 case -0x05:
   retval = free_mem(arguments);
   break;
   /* generated functions */
  {% for name in fn %}
 case {{forloop.counter0}}:
   retval = niftycall_{{name}}(arguments);
   break;
    {% endfor %}
  }
 if (retval!=-1) {
   save_serial(nifty_buffer, retval);
 } else {
   retval = snprintf(nifty_buffer, 100, "badarg\n");
   save_serial(nifty_buffer, retval);
 }
{% endwith %}
}
