{% if "struct" in type %}{% include "from_erl/struct.tpl" %}{% else %}{% with tplname="from_erl/"|add:type|add:".tpl" %}{% include tplname %}{% endwith %}{% endif %}