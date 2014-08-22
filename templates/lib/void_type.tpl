{################################################################}
{% if phase=="to_erl"%}
   forward = snprintf({{buffer}}, {{left}}, "ok");
{% endif %}
{################################################################}
{% if phase=="erlconvert" %}
case R=:="ok\n" of
     true -> ok;
     _ -> fail
end
{% endif %}
