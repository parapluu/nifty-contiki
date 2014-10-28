/*  -------------------------------------------------------------------
	Copyright (c) 2014, Andreas Löscher <andreas.loscher@it.uu.se> and
 	All rights reserved.

 	This file is distributed under the Simplified BSD License.
 	Details can be found in the LICENSE file.
    ------------------------------------------------------------------- */

package se.uu.it.parapluu.cooja_node;

import java.util.Observable;
import java.util.Observer;
import java.util.concurrent.ConcurrentHashMap;

import org.contikios.cooja.interfaces.SerialPort;

public class SerialObserver implements Observer {

	private SerialPort serial_port;
	private ConcurrentHashMap<Integer, String> motes_output;
	private int id;

	public SerialObserver(ConcurrentHashMap<Integer, String> motes_output,
			SerialPort serial_port, int id) {
		super();
		this.motes_output = motes_output;
		this.serial_port = serial_port;
		this.id = id;
	}

	@Override
	public void update(Observable arg0, Object arg1) {
		char c = (char) serial_port.getLastSerialData();
		String data;
		String new_data;

		do {
			data = motes_output.get(id);
			if (data == null) {
				new_data = "" + c;
			} else {
				new_data = data + c;
			}
		} while (!motes_output.replace(id, data, new_data));
	}

}
