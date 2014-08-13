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

static void
write_mem(char* stream) {
  char* ptr;
  char* data;
  ptr = (char*)strtoull(stream, &data, 16);
  data++;
  while (((*data)!='\n') && ((*data)!=0)) {
    *ptr = hexstrtochar(data, &data);
    ptr++;
  }
  printf("ok\n");
}

static void
allocate_mem(char* stream) {
  unsigned long size;
  char* stup;
  void* ptr;
  size = strtoul(stream, &stup, 10);
  ptr = malloc(size);
  printf("%p\n", ptr);
}

static void
read_mem(char* stream) {
  char* ptr;
  unsigned long length;
  unsigned long i;
  ptr = (char*)strtoull(stream, &stream, 16);
  stream++;
  length = strtoul(stream, &stream, 10);
  for (i=0; i<length; i++) {
    printf("%.2X", (unsigned char)ptr[i]);
    if (i<length-1)
      printf(",");
  }
  printf("\n");
}

static void
nifty_sizeof(char* stream) {
  char* typename = stream;
  {% with type_keys=types|fetch_keys %}
	{% for type in type_keys %}
		{% with kind=types|fetch:type|getNth:1 %}
			{% if kind=="base" or kind=="userdef" or kind=="typedef" %}
  if (!(strcmp((const char*)typename, "{{type}}"))) {
    printf(SIZEOF_FORMAT, sizeof({{type|discard_restrict}}));
    return;
  }
			{% endif %}
		{% endwith%}
	{% endfor %}
  {% endwith %}
  printf("undef\n");
}

static void
free_mem(char* stream) {
  void* ptr;
  ptr = (void*)strtoull(stream, &stream, 16);
  free(ptr);
  printf("ok\n");
}
