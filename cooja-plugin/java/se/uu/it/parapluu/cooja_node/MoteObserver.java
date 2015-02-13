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

import org.contikios.cooja.Simulation;
import org.contikios.cooja.interfaces.LED;
import org.contikios.cooja.interfaces.Radio;
import org.contikios.cooja.interfaces.Radio.RadioEvent;

import se.uu.it.parapluu.cooja_node.analyzers.PacketAnalyzer;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class MoteObserver implements Observer {
	
	

	private LED led;
	private Radio radio;
	
	private ConcurrentHashMap<Integer, LinkedList<OtpErlangObject>> events;
	private int id;
	private Simulation simulation;

	public MoteObserver(
			ConcurrentHashMap<Integer, LinkedList<OtpErlangObject>> motes_hw_events, LED led,
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
		OtpErlangObject state = null;
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
			state = PacketAnalyzer.make_opt("led", new OtpErlangAtom(r));
		} else if (o.getClass() == radio.getClass()) {
			RadioEvent e = radio.getLastEvent();
			OtpErlangObject[] data = new OtpErlangObject[2];
			data[0] = new OtpErlangAtom("radio");
			if (e == RadioEvent.HW_ON || e == RadioEvent.HW_OFF) {
				if (radio.isRadioOn()) {
					data[1] = new OtpErlangAtom("on");
				} else {
					data[1] = new OtpErlangAtom("off");
				}
			} else if (e == RadioEvent.TRANSMISSION_STARTED) {
				data[1] = new OtpErlangAtom("transmission_started");
			} else if (e == RadioEvent.TRANSMISSION_FINISHED) {
				data[1] = new OtpErlangAtom("transmission_finished");
			} else if (e == RadioEvent.RECEPTION_STARTED) {
				data[1] = new OtpErlangAtom("reception_started");
			} else if (e == RadioEvent.RECEPTION_INTERFERED) {
				data[1] = new OtpErlangAtom("reception_interfered");
			} else if (e == RadioEvent.RECEPTION_FINISHED) {
				data[1] = new OtpErlangAtom("reception_finished");
			} else if (e == RadioEvent.PACKET_TRANSMITTED) {
				data[1] = new OtpErlangAtom("packet_transmitted");
			} else if (e == RadioEvent.CUSTOM_DATA_TRANSMITTED) {
				data[1] = new OtpErlangAtom("custom_data_transmitted");
			} else if (e == RadioEvent.UNKNOWN){
				data[1] = new OtpErlangAtom("unknown");
			} else {
				data[1] = new OtpErlangAtom("undef");
			}
			state = new OtpErlangTuple(data);
		} else {
			state = PacketAnalyzer.make_opt("err", new OtpErlangAtom("internal"));
		}
		OtpErlangObject[] item_data = {
				new OtpErlangLong(simulation.getSimulationTime()),
				state};
		LinkedList<OtpErlangObject> tmp = events.get(id);
		tmp.add(new OtpErlangTuple(item_data));
		events.replace(id, tmp);
	}
}
