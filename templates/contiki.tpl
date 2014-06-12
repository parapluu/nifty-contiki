#include "contiki.h"
#include "dev/serial-line.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

PROCESS(echo_serial, "Arbitrary Documentation String");
AUTOSTART_PROCESSES(&echo_serial);

/* incude "nifty_lib.tpl"
 * 	+ provide functions for memory allocation/writing/reading
 * 	+ provide platform specific information (size of types)
 */


/* include "structures.tpl"
 * 	+ provide translations for each struct type from string to struct and from struct to string
 */

{% include "function.tpl" %}

PROCESS_THREAD(echo_serial, ev, data)
{
  PROCESS_BEGIN();
  printf("Here we go\n");
  for (;;) {
    PROCESS_WAIT_EVENT_UNTIL(ev == serial_line_event_message && data != NULL);
    process_input((char*)data);
  }
  PROCESS_END();
}
