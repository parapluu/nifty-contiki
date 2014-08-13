#include "contiki.h"
#include "dev/serial-line.h"
#include "dev/uart1.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "{{header|absname}}"

#ifdef __MSP430__
#define strtoll(a,b,c) strtol((a),(b),(c))
#define strtoull(a,b,c) strtoul((a),(b),(c))
#define SIZEOF_FORMAT "%u\n"
#else
#define SIZEOF_FORMAT "%lu\n"
#endif

PROCESS({{module}}, "Process {{module}}");
AUTOSTART_PROCESSES(&{{module}});

/*
 * 	+ provide platform specific information (size of types)
 */
{% include "nifty_lib.tpl" %}


/* include "structures.tpl"
 * 	+ provide translations for each struct type from string to struct and from struct to string
 */

{% include "function.tpl" %}

PROCESS_THREAD({{module}}, ev, data)
{
  PROCESS_BEGIN();
  /* UIP hack */
#if !(!WITH_UIP && !WITH_UIP6)
  uart1_set_input(serial_line_input_byte);
  serial_line_init();
#endif
  for (;;) {
    PROCESS_WAIT_EVENT_UNTIL(ev == serial_line_event_message && data != NULL);
    process_input((char*)data);
  }
  PROCESS_END();
}
