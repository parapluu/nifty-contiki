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
    {{type}} c_retval;
  {% endif %}

{% endif %}

{################################################################}
{% if phase=="to_c" %}
    {% if typedef|getNth:2=="unsigned" %}
   {{carg}} = ({{type}})strtoull(curarg, &nextarg, 10);
    {% else %}
   {{carg}} = ({{type}})strtoll(curarg, &nextarg, 10);
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