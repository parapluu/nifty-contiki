{#
{% with args=call|getNth:3 retval=call|getNth:4 %}
   {% if not retval=="" %}
      {% with Id=args|getNth:1 %}
YIELD_THEN_WAIT_UNTIL(id == {{Id}} &amp;&amp; msg.contains("{{retval}}"));
      {% endwith %}
   {% endif %}
{% endwith %}
#}