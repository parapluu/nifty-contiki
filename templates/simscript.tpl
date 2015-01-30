{% for Id in init_motes %}
mote{{Id}}=null;
{% endfor %}

while 
({% for Id in init_motes %}
  (mote{{Id}}==null) ||
{% endfor %}  false) {
{% for Id in init_motes %}
   if (id == {{Id}}) {
      mote{{Id}} = mote;
   }
{% endfor %}
   YIELD();
}

{% for call in calls %}
   {% with type=call|getNth:2 %}
      {% include "scriptlib/calls.tpl" %}
   {% endwith %}
{% endfor %}
