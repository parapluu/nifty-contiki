/*  -------------------------------------------------------------------
	Copyright (c) 2014, Andreas Löscher <andreas.loscher@it.uu.se> and
 	All rights reserved.

 	This file is distributed under the Simplified BSD License.
 	Details can be found in the LICENSE file.
    ------------------------------------------------------------------- */

package se.uu.it.parapluu.cooja_node;

import java.util.LinkedList;
import java.util.Objects;
import java.util.Observable;
import java.util.Observer;
import java.util.concurrent.ConcurrentHashMap;

import org.contikios.cooja.Simulation;
import org.contikios.cooja.interfaces.LED;
import org.contikios.cooja.interfaces.Radio;
import org.contikios.cooja.interfaces.Radio.RadioEvent;

public class MoteObserver implements Observer {
	
	

	private LED led;
	private Radio radio;
	
	private ConcurrentHashMap<Integer, LinkedList<String[]>> events;
	private int id;
	private Simulation simulation;

	public MoteObserver(
			ConcurrentHashMap<Integer, LinkedList<String[]>> motes_hw_events, LED led,
			Radio radio, int id, Simulation simulation) {
		super();
		this.led = led;
		this.radio = radio;
		this.events = motes_hw_events;
		this.id = id;
		this.simulation = simulation;
	}

	@Override
	public void update(Observable o, Object arg) {
		if (o.getClass() == led.getClass()) {
			String r = "";
			if (led.isYellowOn()) {
				r += "y";
			}
			if (led.isGreenOn()) {
				r += "g";
			}
			if (led.isRedOn()) {
				r += "r";
			}
			String[] rv = new String[3];
			rv[0] = "led";
			rv[1] = r;
			rv[2] = Objects.toString(simulation.getSimulationTime());
			LinkedList<String[]> tmp = events.get(id); 
			tmp.add(rv);
			events.replace(id, tmp);
		} else if (o.getClass() == radio.getClass()) {
			RadioEvent e = radio.getLastEvent();
			String[] rv = new String[3];
			rv[0] = "radio";
			rv[2] = Objects.toString(simulation.getSimulationTime());
			if (e == RadioEvent.HW_ON || e == RadioEvent.HW_OFF) {
				if (radio.isRadioOn()) {
					rv[1] = "on";
				} else {
					rv[1] = "off";
				}
			} else if (e == RadioEvent.TRANSMISSION_STARTED) {
				rv[1] = "transmission_started";
			} else if (e == RadioEvent.TRANSMISSION_FINISHED) {
				rv[1] = "transmission_finished";
			} else if (e == RadioEvent.RECEPTION_STARTED) {
				rv[1] = "reception_started";
			} else if (e == RadioEvent.RECEPTION_INTERFERED) {
				rv[1] = "reception_interfered";
			} else if (e == RadioEvent.RECEPTION_FINISHED) {
				rv[1] = "reception_finished";
			} else if (e == RadioEvent.PACKET_TRANSMITTED) {
				rv[1] = "packet_transmitted";
			} else if (e == RadioEvent.CUSTOM_DATA_TRANSMITTED) {
				rv[1] = "custom_data_transmitted";
			} else if (e == RadioEvent.UNKNOWN){
				rv[1] = "unknown";
			} else {
				rv[1] = "undef";
			}
			LinkedList<String[]> tmp = events.get(id); 
			tmp.add(rv);
			events.put(id, tmp);
		} else {
			LinkedList<String[]> tmp = events.get(id);
			String[] rv = new String[2];
			rv[0] = "err";
			rv[1] = "internal";
			tmp.add(rv);
			events.put(id, tmp);
		}
	}
}
