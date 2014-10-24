{################################################################}
{% if phase=="prepare" %}
      {{raw_type}}* {{carg}};
{% endif %}

{################################################################}
{% if phase=="to_c" %}
      {{carg}} = record_to_ptr_{{types|fetch:type|getNth:2|getNth:1|getNth:2}}(curarg, &nextarg);
{% endif %}

{################################################################}
{% if phase=="argument" %}
  {% if argument|is_argument %}
     ({{raw_type|discard_restrict}})(*{{carg}})
  {% else %}
     ({{type}})
  {% endif %}
{% endif %}

{################################################################}
{% if phase=="to_erl"%}
forward = ptr_to_record_{{types|fetch:type|getNth:2|getNth:1|getNth:2}}({{buffer}}, {{left}}, &{{carg}});
{% endif %}

{################################################################}
{% if phase=="erlformat" %}
build_struct_fmt({{type}})
{% endif %}

{################################################################}
{% if phase=="erlconvert" %}
build_record(string:substr(R, 1, length(R)-1))
{% endif %}
