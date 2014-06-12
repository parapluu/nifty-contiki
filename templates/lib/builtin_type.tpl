{% with type_tuple=types|fetch:type %}
	{% with kind=type_tuple|getNth:1 typedef=type_tuple|getNth:2 %}
		{% if "int" == typedef|getNth:1 or "char" == typedef|getNth:1%}
			{% include "lib/int_type.tpl" %}
		{% else %}
			{% include "lib/pointer_type.tpl" %}
		{% endif %}
	{% endwith %}
{% endwith %}
