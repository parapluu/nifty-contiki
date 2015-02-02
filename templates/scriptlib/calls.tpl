{% with delay=call|getNth:2 args=call|getNth:4 %}
   {% if delay>0 %}
      GENERATE_MSG({{delay|niceint}}, "continue");
      YIELD_THEN_WAIT_UNTIL(msg.equals("continue"));
   {% endif %}
{% endwith %}
{% if "mote_write"==type %}
   {% include "scriptlib/mote_write.tpl" %}
{% else %}{% if "mote_read"==type %}
   {% include "scriptlib/mote_read.tpl" %}
{% else %}
{# /* ignored {{type}} */ #}
{% endif %}{% endif %}
