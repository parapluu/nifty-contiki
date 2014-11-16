{################################################################}
{% if phase=="prepare" %}

  {% if argument|is_argument %}
    {% if typedef|getNth:2=="unsigned" %}
      unsigned long {{carg}};
    {% else %}
      long {{carg}};
    {% endif %}
  {% endif %}

  {% if argument|is_return %}
    {% if typedef|getNth:2=="unsigned" %}
    unsigned long c_retval;
    {% else %}
    long c_retval;
    {% endif %}
  {% endif %}

{% endif %}

{################################################################}
{% if phase=="to_c" %}
    {% if typedef|getNth:2=="unsigned" %}
   {{carg}} = ({{type}})strtoull(curarg, &nextarg, 16);
    {% else %}
   {{carg}} = ({{type}})strtoll(curarg, &nextarg, 16);
    {% endif %}
{% endif %}

{################################################################}
{% if phase=="argument" %}
  {% if argument|is_argument %}
    ({{raw_type|discard_restrict}}){{carg}}
  {% else %}
    ({{type}})
  {% endif %}
{% endif %}

{################################################################}
{% if phase=="to_erl"%}
  {% if typedef|getNth:2=="unsigned" %}
    forward = snprintf({{buffer}}, {{left}}, "%lu", (unsigned long){{carg}});
  {% else %}
    forward = snprintf({{buffer}}, {{left}}, "%ld", (long){{carg}});
  {% endif %}
{% endif %}

{################################################################}
{# no cleanup phase #}

{################################################################}
{% if phase=="erlformat" %}
"~.16b "
{% endif %}
{################################################################}
{% if phase=="erlconvert" %}
list_to_integer(string:substr(R, 1, length(R)-1), 10)
{% endif %}
