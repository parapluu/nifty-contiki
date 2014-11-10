/*  -------------------------------------------------------------------
	Copyright (c) 2014, Andreas Löscher <andreas.loscher@it.uu.se> and
 	All rights reserved.

 	This file is distributed under the Simplified BSD License.
 	Details can be found in the LICENSE file.
    ------------------------------------------------------------------- */

package se.uu.it.parapluu.cooja_node;

import java.util.LinkedList;
import java.util.Observable;
import java.util.Observer;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.log4j.Logger;
import org.contikios.cooja.interfaces.SerialPort;

public class SerialObserver implements Observer {
	private static Logger logger = Logger.getLogger(MessageHandler.class);
	private SerialPort serial_port;
	private ConcurrentHashMap<Integer, String> cache;
	private ConcurrentHashMap<Integer, LinkedList<String>> messages;
	public ConcurrentHashMap<Integer, LinkedList<String>> getEvents() {
		return events;
	}

	public ConcurrentHashMap<Integer, LinkedList<String>> getMessages() {
		return messages;
	}

	private ConcurrentHashMap<Integer, LinkedList<String>> events;
	private int id;

	public SerialObserver(ConcurrentHashMap<Integer, String> cache,
			ConcurrentHashMap<Integer, LinkedList<String>> messages,
			ConcurrentHashMap<Integer, LinkedList<String>> events, 
			SerialPort serial_port, int id) {
		super();
		this.cache = cache;
		this.messages = messages;
		this.events = events;
		this.serial_port = serial_port;
		this.id = id;
	}

	@Override
	public void update(Observable arg0, Object arg1) {
		char c = (char) serial_port.getLastSerialData();
		String data;
		String new_data;

		if (c=='\n') {
			/* new message */
			data = cache.get(id) + c;
			if (data.startsWith("EVENT")) {
				/* it's an event */
				LinkedList<String> tmp = events.get(id);
				tmp.addLast(data);
				events.put(id, tmp);
			} else {
				/* it's a message */
				LinkedList<String> tmp = messages.get(id);
				tmp.addLast(data);
				messages.put(id, tmp);
			}
			new_data = "";
		} else {
			/* append */
			new_data = cache.get(id)+c;
		}
		cache.put(id, new_data);
	}
}
