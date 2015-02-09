/*  -------------------------------------------------------------------
	Copyright (c) 2014, Andreas Löscher <andreas.loscher@it.uu.se>
 	All rights reserved.

 	This file is distributed under the Simplified BSD License.
 	Details can be found in the LICENSE file.
    ------------------------------------------------------------------- */
package se.uu.it.parapluu.cooja_node;

import java.io.IOException;
import java.util.LinkedList;
import java.util.Observer;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

import org.apache.log4j.Logger;
import org.contikios.cooja.Mote;
import org.contikios.cooja.MoteType;
import org.contikios.cooja.RadioMedium;
import org.contikios.cooja.SimEventCentral.LogOutputEvent;
import org.contikios.cooja.SimEventCentral.LogOutputListener;
import org.contikios.cooja.Simulation;
import org.contikios.cooja.TimeEvent;
import org.contikios.cooja.interfaces.LED;
import org.contikios.cooja.interfaces.Radio;
import org.contikios.cooja.interfaces.SerialPort;
import org.contikios.cooja.mote.memory.MemoryInterface.Symbol;
import org.contikios.cooja.mote.memory.VarMemory;
import org.contikios.cooja.radiomediums.UDGM;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class MessageHandler extends Thread {
	private static Logger logger = Logger.getLogger(MessageHandler.class);
	private Simulation simulation;
	private OtpConnection conn;

	private ConcurrentHashMap<Integer, Observer> motes_observer;

	private ConcurrentHashMap<Integer, LinkedList<String[]>> motes_hw_events;
	private ConcurrentHashMap<Integer, Observer> motes_hw_observer;

	private String wait_msg;
	private ReentrantLock wait_lock;

	private LogOutputListener logOutputListener = new LogOutputListener() {
		public void moteWasAdded(Mote mote) {
		}

		public void moteWasRemoved(Mote mote) {
		}

		public void newLogOutput(LogOutputEvent ev) {
			handleOutput(ev.msg);
		}

		public void removedLogOutput(LogOutputEvent ev) {
		}
	};
	private LinkedList<OtpErlangObject> radio_messages;
	private RadioObserver radio_observer;

	public MessageHandler(OtpConnection conn, OtpErlangPid pid,
			Simulation simulation) {
		this.conn = conn;
		this.simulation = simulation;
		this.motes_observer = new ConcurrentHashMap<Integer, Observer>();
		this.motes_hw_events = new ConcurrentHashMap<Integer, LinkedList<String[]>>();
		this.motes_hw_observer = new ConcurrentHashMap<Integer, Observer>();
		this.wait_msg = null;
		this.wait_lock = new ReentrantLock();
		this.wait_lock.lock();
		OtpErlangObject[] msg = { new OtpErlangAtom("pid"), pid };
		try {
			this.conn.send("cooja_master", new OtpErlangTuple(msg));
		} catch (IOException e) {
			throw (RuntimeException) new RuntimeException("Node error: "
					+ e.getMessage()).initCause(e);
		}
	}

	protected void handleOutput(String msg) {
		if (this.wait_msg == msg) {
			this.wait_lock.unlock();
		}
	}

	private void wait_for_msg(String msg) {
		this.wait_msg = msg;
		this.wait_lock.lock();
	}

	public void run() {
		OtpErlangTuple msg;
		OtpErlangPid sender;
		OtpErlangAtom request;
		this.simulation.getEventCentral().addLogOutputListener(
				logOutputListener);
		while (this.conn.isAlive()) {
			try {
				msg = (OtpErlangTuple) conn.receive();
				// logger.fatal(msg.toString());
				sender = (OtpErlangPid) msg.elementAt(0);
				request = (OtpErlangAtom) msg.elementAt(1);
				handleRequest(sender, request, msg);
			} catch (OtpErlangExit e) {
				break;
			} catch (OtpAuthException e) {
				throw (RuntimeException) new RuntimeException("Node error: "
						+ e.getMessage()).initCause(e);
			} catch (IOException e) {
				throw (RuntimeException) new RuntimeException("Node error: "
						+ e.getMessage()).initCause(e);
			} catch (InterruptedException e) {
				throw (RuntimeException) new RuntimeException("Node error: "
						+ e.getMessage()).initCause(e);
			}
		}
	}

	private void sendResponsWithTime(OtpErlangPid sender,
			OtpErlangObject response) throws IOException {
		long time = this.simulation.getSimulationTime();
		OtpErlangObject items[] = new OtpErlangObject[2];
		items[0] = new OtpErlangLong(time);
		items[1] = response;
		this.conn.send(sender, new OtpErlangTuple(items));
	}

	private void handleRequest(OtpErlangPid sender, OtpErlangAtom request,
			OtpErlangTuple msg) throws InterruptedException {
		try {
			switch (request.atomValue()) {
			case "quit_cooja":
				handle_quit_cooja(sender);
				break;
			case "start_simulation":
				handle_start_simulation(sender);
				break;
			case "stop_simulation":
				handle_stop_simulation(sender);
				break;
			case "set_speed_limit":
				handle_set_speed_limit(sender, msg);
				break;
			case "set_random_seed":
				handle_set_random_seed(sender, msg);
				break;
			case "is_running":
				handle_is_running(sender);
				break;
			case "simulation_time":
				handle_simulation_time(sender);
				break;
			case "simulation_time_ms":
				handle_simulation_time_ms(sender);
				break;
			case "simulation_step_ms":
				handle_simulation_step_ms(sender);
				break;
			case "simulation_step":
				handle_simulation_step(sender, msg);
				break;
			case "radio_set_config":
				handle_radio_set_config(sender, msg);
				break;
			case "radio_get_config":
				handle_radio_get_config(sender);
				break;
			case "radio_listen":
				handle_radio_listen(sender, msg);
				break;
			case "radio_unlisten":
				handle_radio_unlisten(sender);
				break;
			case "radio_get_messages":
				handle_radio_get_messages(sender);
				break;
			case "motes":
				handle_motes(sender);
				break;
			case "mote_types":
				handle_mote_types(sender);
				break;
			case "mote_add":
				handle_mote_add(sender, msg);
				break;
			case "mote_del":
				handle_mote_del(sender, msg);
				break;
			case "mote_get_pos":
				handle_mote_get_pos(sender, msg);
				break;
			case "mote_set_pos":
				handle_mote_set_pos(sender, msg);
				break;
			case "mote_write":
				handle_mote_write(sender, msg);
				break;
			case "mote_listen":
				handle_mote_listen(sender, msg);
				break;
			case "mote_hw_listen":
				handle_mote_hw_listen(sender, msg);
				break;
			case "mote_unlisten":
				handle_mote_unlisten(sender, msg);
				break;
			case "mote_hw_unlisten":
				handle_mote_hw_unlisten(sender, msg);
				break;
			case "mote_hw_events":
				handle_mote_hw_events(sender, msg);
				break;
			case "mote_read":
				handle_mote_read(sender, msg);
				break;
			case "mote_mem_read":
				handle_mote_mem_read(sender, msg);
				break;
			case "mote_mem_write":
				handle_mote_mem_write(sender, msg);
				break;
			case "mote_mem_vars":
				handle_mote_mem_vars(sender, msg);
				break;
			case "mote_mem_symbol":
				handle_mote_mem_symbol(sender, msg);
				break;
			case "msg_wait":
				handle_msg_wait(sender, msg);
				break;
			case "get_last_event":
				handle_get_last_event(sender, msg);
				break;
			default: {
				logger.fatal("Undefined message\n");
				sendResponsWithTime(sender, new OtpErlangAtom("undef"));
				break;
			}
			}
		} catch (IOException e) {
			try {
				sendResponsWithTime(sender, new OtpErlangAtom("error"));
			} catch (IOException e1) {
				logger.fatal(e1.getMessage());
				System.exit(1);
			}
			logger.fatal(e.getMessage());
			System.exit(1);
		}
	}

	private void handle_mote_mem_symbol(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException{
		int id;
		OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
		try {
			/* unpack message */
			id = ((OtpErlangLong) args.elementAt(0)).intValue();
			String varname = ((OtpErlangString)(args.elementAt(1))).stringValue(); 
			Mote mote = simulation.getMoteWithID(id);
			/* get memory */
			VarMemory mem = new VarMemory(mote.getMemory());
			if (mem.variableExists(varname)) {
				Symbol sym  = mem.getVariable(varname);
				OtpErlangLong addr = new OtpErlangLong(sym.addr);
				OtpErlangLong size = new OtpErlangLong(sym.size);
				OtpErlangString name = new OtpErlangString(sym.name);
				OtpErlangObject retval[] = new OtpErlangObject[3];
				retval[0] = addr;
				retval[1] = size;
				retval[2] = name;
				sendResponsWithTime(sender, new OtpErlangTuple(retval));
			} else {
				sendResponsWithTime(sender, new OtpErlangAtom("badname"));	
			}
		} catch (OtpErlangRangeException e) {
			sendResponsWithTime(sender, new OtpErlangAtom("badid"));
		}
	}

	private void handle_mote_mem_vars(OtpErlangPid sender, OtpErlangTuple msg) 
			throws IOException{
		int id;
		OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
		try {
			/* unpack message */
			id = ((OtpErlangLong) args.elementAt(0)).intValue();
			Mote mote = simulation.getMoteWithID(id);
			/* get memory */
			VarMemory mem = new VarMemory(mote.getMemory());
			
			Set<String> varnames = mem.getVariableNames();
			OtpErlangObject retval[] = new OtpErlangObject[varnames.size()];
			int i=0;
			for (String name : varnames) {
				retval[i++] = new OtpErlangString(name);
			}
			sendResponsWithTime(sender, new OtpErlangList(retval));
		} catch (OtpErlangRangeException e) {
			sendResponsWithTime(sender, new OtpErlangAtom("badid"));
		}
	}

	private void handle_mote_mem_write(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		int id;
		OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
		try {
			/* unpack message */
			id = ((OtpErlangLong) args.elementAt(0)).intValue();
			OtpErlangTuple var_obj = ((OtpErlangTuple) args.elementAt(1));
			long addr = ((OtpErlangLong) var_obj.elementAt(0)).longValue();
			int size = ((OtpErlangLong) var_obj.elementAt(1)).intValue();
			byte data[] = ((OtpErlangBinary)args.elementAt(2)).binaryValue();
			if (size!=data.length) {
				sendResponsWithTime(sender, new OtpErlangAtom("size mismatch"));
				return;
			}
			/* get mote memory */
			Mote mote = simulation.getMoteWithID(id);
			VarMemory mem = new VarMemory(mote.getMemory());
			/* write */
			mem.setByteArray(addr, data);
			sendResponsWithTime(sender, new OtpErlangAtom("ok"));
		} catch (OtpErlangRangeException e) {
			sendResponsWithTime(sender, new OtpErlangAtom("badid"));
		}
	}

	private void handle_mote_mem_read(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		int id;
		OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
		try {
			/* unpack message */
			id = ((OtpErlangLong) args.elementAt(0)).intValue();
			OtpErlangTuple var_obj = ((OtpErlangTuple) args.elementAt(1));
			long addr = ((OtpErlangLong) var_obj.elementAt(0)).longValue();
			int size = ((OtpErlangLong) var_obj.elementAt(1)).intValue();
			/* get mote memory */
			Mote mote = simulation.getMoteWithID(id);
			VarMemory mem = new VarMemory(mote.getMemory());
			byte data[] = mem.getByteArray(addr, size);
			sendResponsWithTime(sender, new OtpErlangBinary(data));
		} catch (OtpErlangRangeException e) {
			sendResponsWithTime(sender, new OtpErlangAtom("badid"));
		}
	}

	private void handle_get_last_event(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		int id;
		OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
		try {
			id = ((OtpErlangLong) args.elementAt(0)).intValue();
			if (!motes_observer.containsKey(id)) {
				sendResponsWithTime(sender, new OtpErlangAtom("not_listened"));
			} else {
				String retval = ((SerialObserver) (motes_observer.get(id)))
						.nextEvent();
				if (retval == null) {
					sendResponsWithTime(sender, new OtpErlangAtom("no_event"));
				} else {
					sendResponsWithTime(sender, new OtpErlangList(retval));
				}
			}
		} catch (OtpErlangRangeException e) {
			sendResponsWithTime(sender, new OtpErlangAtom("badid"));
		}
	}

	private void handle_msg_wait(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		String data;
		OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
		data = ((OtpErlangString) args.elementAt(0)).stringValue();
		wait_for_msg(data);
		sendResponsWithTime(sender, new OtpErlangAtom("ok"));
	}

	private void handle_mote_hw_events(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		int id;
		try {
			OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
			id = ((OtpErlangLong) args.elementAt(0)).intValue();
			if (motes_hw_events.containsKey(id)) {
				LinkedList<String[]> empty = new LinkedList<String[]>();
				LinkedList<String[]> l = motes_hw_events.replace(id, empty);
				OtpErlangObject[] elements = new OtpErlangObject[l.size()];
				for (int i = 0; i < l.size(); i++) {
					String[] data = l.get(i);
					OtpErlangAtom key = new OtpErlangAtom(data[0]);
					OtpErlangObject[] value_elements = new OtpErlangObject[data.length - 1];
					for (int j = 1; j < data.length; j++) {
						value_elements[j - 1] = (OtpErlangObject) (new OtpErlangList(
								data[j]));
					}
					OtpErlangList val = new OtpErlangList(value_elements);
					OtpErlangObject[] element = new OtpErlangObject[2];
					element[0] = key;
					element[1] = val;
					elements[i] = (OtpErlangObject) (new OtpErlangTuple(element));
				}
				sendResponsWithTime(sender, new OtpErlangList(elements));
			} else {
				this.conn.send(sender, new OtpErlangAtom("not_listened_on"));
			}
		} catch (OtpErlangRangeException e) {
			sendResponsWithTime(sender, new OtpErlangAtom("badid"));
		}
	}

	private void handle_mote_hw_unlisten(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		int id;
		try {
			OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
			id = ((OtpErlangLong) args.elementAt(0)).intValue();
			if (motes_hw_observer.containsKey(id)) {
				Observer obs = this.motes_hw_observer.get(id);
				Mote mote = this.simulation.getMoteWithID(id);
				LED led = (LED) mote.getInterfaces().getLED();
				Radio radio = (Radio) mote.getInterfaces().getRadio();
				led.deleteObserver(obs);
				radio.deleteObserver(obs);
				motes_hw_observer.remove(id);
				motes_hw_events.remove(id);
				sendResponsWithTime(sender, new OtpErlangAtom("ok"));
			} else {
				this.conn.send(sender, new OtpErlangAtom("not_listened_on"));
			}
		} catch (OtpErlangRangeException e) {
			sendResponsWithTime(sender, new OtpErlangAtom("badid"));
		}
	}

	private void handle_mote_unlisten(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		int id;
		try {
			OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
			id = ((OtpErlangLong) args.elementAt(0)).intValue();
			if (motes_observer.containsKey(id)) {
				Observer obs = this.motes_observer.get(id);
				Mote mote = this.simulation.getMoteWithID(id);
				SerialPort serial_port = (SerialPort) mote.getInterfaces()
						.getLog();
				serial_port.deleteSerialDataObserver(obs);
				motes_observer.remove(id);
				sendResponsWithTime(sender, new OtpErlangAtom("ok"));
			} else {
				this.conn.send(sender, new OtpErlangAtom("not_listened_on"));
			}
		} catch (OtpErlangRangeException e) {
			sendResponsWithTime(sender, new OtpErlangAtom("badid"));
		}
	}

	private void handle_mote_hw_listen(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		int id;
		try {
			OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
			id = ((OtpErlangLong) args.elementAt(0)).intValue();
			if (!motes_hw_observer.containsKey(id)) {
				Mote mote = this.simulation.getMoteWithID(id);
				LED led = (LED) mote.getInterfaces().getLED();
				Radio radio = (Radio) mote.getInterfaces().getRadio();
				MoteObserver obs = new MoteObserver(motes_hw_events, led,
						radio, id, simulation);
				if (led != null) {
					led.addObserver(obs);
				}
				if (radio != null) {
					radio.addObserver(obs);
				}
				motes_hw_observer.put(id, obs);
				motes_hw_events.put(id, new LinkedList<String[]>());
				sendResponsWithTime(sender, new OtpErlangAtom("ok"));
			} else {
				sendResponsWithTime(sender, new OtpErlangAtom(
						"already_listened_on"));
			}
		} catch (OtpErlangRangeException e) {
			sendResponsWithTime(sender, new OtpErlangAtom("badid"));
		}
	}

	private void handle_mote_listen(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		int id;
		try {
			OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
			id = ((OtpErlangLong) args.elementAt(0)).intValue();
			if (!motes_observer.containsKey(id)) {
				Mote mote = this.simulation.getMoteWithID(id);
				SerialPort serial_port = (SerialPort) mote.getInterfaces()
						.getLog();
				SerialObserver obs = new SerialObserver(serial_port);
				motes_observer.put(id, obs);
				serial_port.addSerialDataObserver(obs);
				sendResponsWithTime(sender, new OtpErlangAtom("ok"));
			} else {
				sendResponsWithTime(sender, new OtpErlangAtom(
						"already_listened_on"));
			}
		} catch (OtpErlangRangeException e) {
			sendResponsWithTime(sender, new OtpErlangAtom("badid"));
		}
	}

	private void handle_mote_write(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		int id;
		String data;
		try {
			OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
			id = ((OtpErlangLong) args.elementAt(0)).intValue();
			data = ((OtpErlangString) args.elementAt(1)).stringValue();
			Mote mote = this.simulation.getMoteWithID(id);
			SerialPort serialPort = (SerialPort) mote.getInterfaces().getLog();
			serialPort.writeString(data);
			sendResponsWithTime(sender, new OtpErlangAtom("ok"));
		} catch (OtpErlangRangeException e) {
			sendResponsWithTime(sender, new OtpErlangAtom("badid"));
		} catch (Exception e) {
			sendResponsWithTime(sender, new OtpErlangAtom("badargs"));
		}
	}

	private void handle_motes(OtpErlangPid sender) throws IOException {
		Mote[] motes = this.simulation.getMotes();
		OtpErlangObject[] elements = new OtpErlangObject[motes.length];
		for (int i = 0; i < motes.length; i++) {
			elements[i] = new OtpErlangInt(motes[i].getID());
		}
		sendResponsWithTime(sender, new OtpErlangList(elements));
	}

	private void handle_mote_set_pos(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		int id;
		double x, y, z;
		try {
			OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
			id = ((OtpErlangLong) args.elementAt(0)).intValue();
			x = ((OtpErlangDouble) args.elementAt(1)).doubleValue();
			y = ((OtpErlangDouble) args.elementAt(2)).doubleValue();
			z = ((OtpErlangDouble) args.elementAt(3)).doubleValue();
			try {
				Mote m = simulation.getMoteWithID(id);
				m.getInterfaces().getPosition().setCoordinates(x, y, z);
				sendResponsWithTime(sender, new OtpErlangAtom("ok"));
			} catch (Exception e) {
				sendResponsWithTime(sender, new OtpErlangAtom("fail"));
			}
		} catch (OtpErlangRangeException e) {
			sendResponsWithTime(sender, new OtpErlangAtom("badid"));
		}
	}

	private void handle_mote_get_pos(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		int id;
		try {
			OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
			id = ((OtpErlangLong) args.elementAt(0)).intValue();
			try {
				Mote m = simulation.getMoteWithID(id);
				double x = m.getInterfaces().getPosition().getXCoordinate();
				double y = m.getInterfaces().getPosition().getYCoordinate();
				double z = m.getInterfaces().getPosition().getZCoordinate();
				OtpErlangObject[] retval = new OtpErlangObject[3];
				retval[0] = new OtpErlangDouble(x);
				retval[1] = new OtpErlangDouble(y);
				retval[2] = new OtpErlangDouble(z);
				sendResponsWithTime(sender, new OtpErlangTuple(retval));
			} catch (Exception e) {
				sendResponsWithTime(sender, new OtpErlangAtom("fail"));
			}
		} catch (OtpErlangRangeException e) {
			sendResponsWithTime(sender, new OtpErlangAtom("badid"));
		}
	}

	private void handle_mote_del(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		int id;
		try {
			OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
			id = ((OtpErlangLong) args.elementAt(0)).intValue();
			try {
				Mote m = simulation.getMoteWithID(id);
				simulation.removeMote(m);
				sendResponsWithTime(sender, new OtpErlangAtom("ok"));
			} catch (Exception e) {
				sendResponsWithTime(sender, new OtpErlangAtom("fail"));
			}
		} catch (OtpErlangRangeException e) {
			sendResponsWithTime(sender, new OtpErlangAtom("badid"));
		}
	}

	private void handle_mote_add(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		String id;
		OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
		id = ((OtpErlangString) args.elementAt(0)).stringValue();
		MoteType type = simulation.getMoteType(id);
		if (type != null) {
			Mote nm = type.generateMote(simulation);
			int nextMoteID = 1;
			for (Mote m : simulation.getMotes()) {
				int existing = m.getID();
				if (existing >= nextMoteID) {
					nextMoteID = existing + 1;
				}
			}
			nm.getInterfaces().getMoteID().setMoteID(nextMoteID);
			simulation.addMote(nm);
			int mid = nm.getID();
			OtpErlangObject[] retval = new OtpErlangObject[2];
			retval[0] = new OtpErlangAtom("ok");
			retval[1] = new OtpErlangInt(mid);
			sendResponsWithTime(sender, new OtpErlangTuple(retval));
		} else {
			sendResponsWithTime(sender, new OtpErlangAtom("unknown_type"));
		}
	}

	private void handle_mote_types(OtpErlangPid sender) throws IOException {
		MoteType[] types = simulation.getMoteTypes();
		OtpErlangObject[] retval = new OtpErlangObject[types.length];
		for (int i = 0; i < types.length; i++) {
			retval[i] = new OtpErlangList(types[i].getIdentifier());
		}
		sendResponsWithTime(sender, new OtpErlangList(retval));
	}

	private void handle_radio_get_messages(OtpErlangPid sender)
			throws IOException {
		OtpErlangObject[] retval = new OtpErlangObject[radio_messages.size()];
		for (int i = 0; i < radio_messages.size(); i++) {
			retval[i] = radio_messages.get(i);
		}
		radio_messages.clear();
		sendResponsWithTime(sender, new OtpErlangList(retval));
	}

	private void handle_radio_unlisten(OtpErlangPid sender) throws IOException {
		if (radio_observer == null) {
			sendResponsWithTime(sender, new OtpErlangAtom("not_listened_on"));
		} else {
			RadioMedium rm = simulation.getRadioMedium();
			rm.deleteRadioMediumObserver(radio_observer);
			radio_observer = null;
			radio_messages = null;
			sendResponsWithTime(sender, new OtpErlangAtom("ok"));
		}
	}

	private void handle_radio_listen(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		int analyzer;
		try {
			OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
			analyzer = ((OtpErlangLong) args.elementAt(0)).intValue();
			if (radio_observer == null) {
				RadioMedium rm = simulation.getRadioMedium();
				radio_messages = new LinkedList<OtpErlangObject>();
				radio_observer = new RadioObserver(simulation, radio_messages,
						rm, analyzer);
				rm.addRadioMediumObserver(radio_observer);
				sendResponsWithTime(sender, new OtpErlangAtom("ok"));
			} else {
				sendResponsWithTime(sender, new OtpErlangAtom(
						"already_listened_on"));
			}
		} catch (OtpErlangRangeException e) {
			sendResponsWithTime(sender, new OtpErlangAtom("badid"));
		}
	}

	private void handle_radio_get_config(OtpErlangPid sender)
			throws IOException {
		OtpErlangObject[] retval = new OtpErlangObject[2];
		String radio = simulation.getRadioMedium().getClass().toString();
		retval[0] = new OtpErlangString(radio);
		if (radio.equals(UDGM.class.toString())) {
			UDGM medium = (UDGM) simulation.getRadioMedium();
			// alloc
			OtpErlangObject[] opts = new OtpErlangObject[7];
			OtpErlangObject[] o1 = new OtpErlangObject[2];
			OtpErlangObject[] o2 = new OtpErlangObject[2];
			OtpErlangObject[] o3 = new OtpErlangObject[2];
			OtpErlangObject[] o4 = new OtpErlangObject[2];
			OtpErlangObject[] o5 = new OtpErlangObject[2];
			OtpErlangObject[] o6 = new OtpErlangObject[2];
			OtpErlangObject[] o7 = new OtpErlangObject[2];
			// fill
			o1[0] = new OtpErlangString("COUNTER_INTERFERED");
			o1[1] = new OtpErlangLong(medium.COUNTER_INTERFERED);
			o2[0] = new OtpErlangString("COUNTER_RX");
			o2[1] = new OtpErlangLong(medium.COUNTER_RX);
			o3[0] = new OtpErlangString("COUNTER_TX");
			o3[1] = new OtpErlangLong(medium.COUNTER_TX);
			o4[0] = new OtpErlangString("INTERFERENCE_RANGE");
			o4[1] = new OtpErlangDouble(medium.INTERFERENCE_RANGE);
			o5[0] = new OtpErlangString("SUCCESS_RATIO_RX");
			o5[1] = new OtpErlangDouble(medium.SUCCESS_RATIO_RX);
			o6[0] = new OtpErlangString("SUCCESS_RATIO_TX");
			o6[1] = new OtpErlangDouble(medium.SUCCESS_RATIO_TX);
			o7[0] = new OtpErlangString("TRANSMITTING_RANGE");
			o7[1] = new OtpErlangDouble(medium.TRANSMITTING_RANGE);
			// build
			opts[0] = new OtpErlangTuple(o1);
			opts[1] = new OtpErlangTuple(o2);
			opts[2] = new OtpErlangTuple(o3);
			opts[3] = new OtpErlangTuple(o4);
			opts[4] = new OtpErlangTuple(o5);
			opts[5] = new OtpErlangTuple(o6);
			opts[6] = new OtpErlangTuple(o7);
			retval[1] = new OtpErlangList(opts);
		} else {
			retval[1] = new OtpErlangList();
		}
		sendResponsWithTime(sender, new OtpErlangTuple(retval));
	}

	private void handle_radio_set_config(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
		String radio = ((OtpErlangString) ((OtpErlangTuple) args).elementAt(0))
				.stringValue();
		if (!(simulation.getRadioMedium().getClass().toString().equals(radio))) {
			sendResponsWithTime(sender, new OtpErlangAtom(
					"radio_medium_nomatch"));
		} else {
			if (radio.equals(UDGM.class.toString())) {
				OtpErlangList options = (OtpErlangList) args.elementAt(1);
				try {
					for (OtpErlangObject e : options) {
						String key = ((OtpErlangString) ((OtpErlangTuple) e)
								.elementAt(0)).stringValue();
						UDGM medium = (UDGM) simulation.getRadioMedium();
						switch (key) {
						case "COUNTER_INTERFERED": {
							medium.COUNTER_INTERFERED = ((OtpErlangLong) ((OtpErlangTuple) e)
									.elementAt(1)).intValue();
							break;
						}
						case "COUNTER_RX": {
							medium.COUNTER_RX = ((OtpErlangLong) ((OtpErlangTuple) e)
									.elementAt(1)).intValue();
							break;
						}
						case "COUNTER_TX": {
							medium.COUNTER_TX = ((OtpErlangLong) ((OtpErlangTuple) e)
									.elementAt(1)).intValue();
							break;
						}
						case "INTERFERENCE_RANGE": {
							medium.INTERFERENCE_RANGE = ((OtpErlangDouble) ((OtpErlangTuple) e)
									.elementAt(1)).floatValue();
							break;
						}
						case "SUCCESS_RATIO_RX": {
							medium.SUCCESS_RATIO_RX = ((OtpErlangDouble) ((OtpErlangTuple) e)
									.elementAt(1)).floatValue();
							break;
						}
						case "SUCCESS_RATIO_TX": {
							medium.SUCCESS_RATIO_TX = ((OtpErlangDouble) ((OtpErlangTuple) e)
									.elementAt(1)).floatValue();
							break;
						}
						case "TRANSMITTING_RANGE": {
							medium.TRANSMITTING_RANGE = ((OtpErlangDouble) ((OtpErlangTuple) e)
									.elementAt(1)).floatValue();
							break;
						}
						default: {
							throw new Exception();
						}
						}
					}
					sendResponsWithTime(sender, new OtpErlangAtom("ok"));
				} catch (Exception e) {
					sendResponsWithTime(sender, new OtpErlangAtom(
							"radio_medium_badoptions"));
				}

			} else {
				/* fallback */
				sendResponsWithTime(sender, new OtpErlangAtom(
						"radio_medium_unsupported"));
			}
		}
	}

	private void handle_simulation_step(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException, InterruptedException {
		if (this.simulation.isRunning()) {
			sendResponsWithTime(sender, new OtpErlangAtom("running"));
		} else {
			OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
			long time = ((OtpErlangLong) args.elementAt(0)).longValue();
			TimeEvent stopEvent = new TimeEvent(0) {
				@Override
				public void execute(long arg0) {
					simulation.stopSimulation();
				}
			};
			simulation.scheduleEvent(stopEvent, simulation.getSimulationTime()
					+ (time * Simulation.MILLISECOND));
			simulation.startSimulation();
			while (simulation.isRunning()) {
				sleep(100);
			}
			sendResponsWithTime(sender, new OtpErlangAtom("ok"));
		}
	}

	private void handle_simulation_step_ms(OtpErlangPid sender)
			throws IOException, InterruptedException {
		if (this.simulation.isRunning()) {
			sendResponsWithTime(sender, new OtpErlangAtom("running"));
		} else {
			simulation.stepMillisecondSimulation();
			while (simulation.isRunning()) {
				sleep(1);
			}
			sendResponsWithTime(sender, new OtpErlangAtom("ok"));
		}
	}

	private void handle_simulation_time_ms(OtpErlangPid sender)
			throws IOException {
		sendResponsWithTime(sender,
				new OtpErlangLong(this.simulation.getSimulationTimeMillis()));
	}

	private void handle_simulation_time(OtpErlangPid sender) throws IOException {
		sendResponsWithTime(sender,
				new OtpErlangLong(this.simulation.getSimulationTime()));
	}

	private void handle_is_running(OtpErlangPid sender) throws IOException {
		sendResponsWithTime(sender,
				new OtpErlangAtom(this.simulation.isRunning()));
	}

	private void handle_set_random_seed(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
		long seed = ((OtpErlangLong) args.elementAt(0)).longValue();
		this.simulation.setRandomSeed(seed);
		sendResponsWithTime(sender, new OtpErlangAtom("ok"));
	}

	private void handle_set_speed_limit(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
		double speedlimit = ((OtpErlangDouble) args.elementAt(0)).doubleValue();
		if (speedlimit < 0) {
			this.simulation.setSpeedLimit(null);
		} else {
			this.simulation.setSpeedLimit(speedlimit);
		}
		sendResponsWithTime(sender, new OtpErlangAtom("ok"));
	}

	private void handle_stop_simulation(OtpErlangPid sender) throws IOException {
		this.simulation.stopSimulation();
		sendResponsWithTime(sender, new OtpErlangAtom("ok"));
	}

	private void handle_start_simulation(OtpErlangPid sender)
			throws IOException {
		this.simulation.startSimulation();
		sendResponsWithTime(sender, new OtpErlangAtom("ok"));
	}

	private void handle_quit_cooja(OtpErlangPid sender) throws IOException {
		sendResponsWithTime(sender, new OtpErlangAtom("ok"));
		this.simulation.getCooja().doQuit(false, 0);
	}

	private void handle_mote_read(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		int id;
		try {
			OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
			id = ((OtpErlangLong) args.elementAt(0)).intValue();
		} catch (OtpErlangRangeException e) {
			sendResponsWithTime(sender, new OtpErlangAtom("badid"));
			return;
		}
		if (!motes_observer.containsKey(id)) {
			sendResponsWithTime(sender, new OtpErlangAtom("not_listened"));
		} else {
			String retval = ((SerialObserver) (motes_observer.get(id)))
					.nextMessage();
			if (retval == null) {
				sendResponsWithTime(sender, new OtpErlangList(""));
			} else {
				sendResponsWithTime(sender, new OtpErlangList(retval));
			}
		}
	}
}
