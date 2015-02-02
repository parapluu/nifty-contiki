{% with args=call|getNth:4 %}
   {% with Id=args|getNth:1 Data=args|getNth:2|stripnewline %}
write(mote{{Id}}, "{{Data}}");
   {% endwith %}
{% endwith %}
