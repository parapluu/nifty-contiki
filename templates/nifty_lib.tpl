static char
hexstrtochar(const char* stream, char** next) {
  int i;
  unsigned sum=0, mul=0;
  char* act;
  for (i=0; i<2; i++) {
    act = (char*)(stream + i);
    if (i==1) {
      mul = 1;
    } else {
      mul = 16;
    }
    switch (*act) {
    case '0'...'9':
      sum += ((*act)-48) * mul;
      break;
    case 'a'...'f':
      sum += ((*act)-87) * mul;
      break;
    case 'A'...'F':
      sum += ((*act)-55) * mul;
      break;
    default:
      *next = ((char*)stream + 2);
      return (char)0;
    }
  }
  *next = ((char*)stream + 2);
  return (char)sum;
}

static int
write_mem(char* stream) {
  char* ptr;
  char* data;
  ptr = (char*)strtoull(stream, &data, 16);
  data++;
  while (((*data)!='\n') && ((*data)!=0)) {
    *ptr = hexstrtochar(data, &data);
    ptr++;
  }
  return snprintf(nifty_buffer, 100, "ok");
}

static int
allocate_mem(char* stream) {
  unsigned long size;
  char* stup;
  void* ptr;
  size = strtoul(stream, &stup, 10);
  ptr = malloc(size);
  return snprintf(nifty_buffer, 100, "%p", ptr);
}

static int
read_mem(char* stream) {
  char* ptr;
  unsigned long length;
  unsigned long i;
  unsigned forward;
  ptr = (char*)strtoull(stream, &stream, 16);
  stream++;
  length = strtoul(stream, &stream, 10);
  forward = 0;
  for (i=0; i<length; i++) {
    forward += snprintf(nifty_buffer+forward, 100-forward, "%.2X", (unsigned char)ptr[i]);
    if (i<length-1)
      forward += snprintf(nifty_buffer+forward, 100-forward, ",");
  }
  return forward;
}

static int
nifty_sizeof(char* stream) {
  char* typename = stream;
{% if not config|small_size %}
{% with type_keys=types|fetch_keys %}
	{% for type in type_keys %}
		{% with kind=types|fetch:type|getNth:1 %}
			{% if kind=="base" or kind=="userdef" or kind=="typedef" %}
  if (!(strcmp((const char*)typename, "{{type|discard_restrict}}"))) {
    return snprintf(nifty_buffer, 100, SIZEOF_FORMAT, sizeof({{type|discard_restrict}}));
  }
			{% endif %}
		{% endwith%}
	{% endfor %}
{% endwith %}
{% endif %}
  return snprintf(nifty_buffer, 100, "undef");
}

static int
free_mem(char* stream) {
  void* ptr;
  ptr = (void*)strtoull(stream, &stream, 16);
  free(ptr);
  return snprintf(nifty_buffer, 100, "ok");
}
