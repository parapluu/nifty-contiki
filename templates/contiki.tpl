#include "contiki.h"
#include "dev/serial-line.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "{{header|absname}}"

PROCESS(echo_serial, "Arbitrary Documentation String");
AUTOSTART_PROCESSES(&echo_serial);

/*
 * 	+ provide platform specific information (size of types)
 */
{% include "nifty_lib.tpl" %}


/* include "structures.tpl"
 * 	+ provide translations for each struct type from string to struct and from struct to string
 */

{% include "function.tpl" %}

PROCESS_THREAD(echo_serial, ev, data)
{
  PROCESS_BEGIN();
  for (;;) {
    PROCESS_WAIT_EVENT_UNTIL(ev == serial_line_event_message && data != NULL);
    process_input((char*)data);
  }
  PROCESS_END();
}
