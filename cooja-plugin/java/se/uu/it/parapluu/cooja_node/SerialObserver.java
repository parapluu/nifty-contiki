/*  -------------------------------------------------------------------
	Copyright (c) 2014, Andreas Löscher <andreas.loscher@it.uu.se> and
 	All rights reserved.

 	This file is distributed under the Simplified BSD License.
 	Details can be found in the LICENSE file.
    ------------------------------------------------------------------- */

package se.uu.it.parapluu.cooja_node;

import java.util.Observable;
import java.util.Observer;
import java.util.concurrent.ConcurrentLinkedQueue;

// import org.apache.log4j.Logger;
import org.contikios.cooja.interfaces.SerialPort;

public class SerialObserver implements Observer {
	// private static Logger logger = Logger.getLogger(MessageHandler.class);
	private SerialPort serial_port;
	private String cache;
	private ConcurrentLinkedQueue<String> messages;
	private ConcurrentLinkedQueue<String> events;

	public String nextEvent() {
		return events.poll();
	}

	public String nextMessage() {
		return messages.poll();
	}
	
	public SerialObserver(SerialPort serial_port) {
		super();
		this.cache = "";
		this.messages = new ConcurrentLinkedQueue<String>();
		this.events = new ConcurrentLinkedQueue<String>();
		this.serial_port = serial_port;
	}

	@Override
	public void update(Observable arg0, Object arg1) {
		char c = (char) serial_port.getLastSerialData();
		String data;
		String new_data;

		if (c=='\n') {
			/* new message */
			data = cache + c;
			if (data.startsWith("EVENT")) {
				/* it's an event */
				events.add(data);
				// logger.warn("Event: "+data);
			} else {
				/* it's a message */
				messages.add(data);
				// logger.warn("Message: "+data);
			}
			new_data = "";
		} else {
			/* append */
			new_data = cache+c;
		}
		cache = new_data;
	}
}

