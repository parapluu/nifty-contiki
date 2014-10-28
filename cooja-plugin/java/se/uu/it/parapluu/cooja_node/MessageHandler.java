/*  -------------------------------------------------------------------
	Copyright (c) 2014, Andreas Löscher <andreas.loscher@it.uu.se> and
 	All rights reserved.

 	This file is distributed under the Simplified BSD License.
 	Details can be found in the LICENSE file.
    ------------------------------------------------------------------- */
package se.uu.it.parapluu.cooja_node;

import java.io.IOException;
import java.util.LinkedList;
import java.util.Observer;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;
import org.contikios.cooja.Mote;
import org.contikios.cooja.MoteType;
import org.contikios.cooja.SimEventCentral.LogOutputEvent;
import org.contikios.cooja.SimEventCentral.LogOutputListener;
import org.contikios.cooja.Simulation;
import org.contikios.cooja.TimeEvent;
import org.contikios.cooja.interfaces.LED;
import org.contikios.cooja.interfaces.Radio;
import org.contikios.cooja.interfaces.SerialPort;
import org.contikios.cooja.radiomediums.UDGM;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangAtom;
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

	private ConcurrentHashMap<Integer, String> motes_output;
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

	public MessageHandler(OtpConnection conn, OtpErlangPid pid,
			Simulation simulation) {
		this.conn = conn;
		this.simulation = simulation;
		this.motes_output = new ConcurrentHashMap<Integer, String>();
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

	private void handleRequest(OtpErlangPid sender, OtpErlangAtom request,
			OtpErlangTuple msg) throws InterruptedException {
		try {
			switch (request.atomValue()) {
			/*
			 * Cooja Stuff
			 */
			case "quit_cooja":
				this.conn.send(sender, new OtpErlangAtom("ok"));
				this.simulation.getCooja().doQuit(false, 0);
				break;
			/*
			 * Simulation specific calls
			 */
			case "start_simulation":
				this.simulation.startSimulation();
				this.conn.send(sender, new OtpErlangAtom("ok"));
				break;
			case "stop_simulation":
				this.simulation.stopSimulation();
				this.conn.send(sender, new OtpErlangAtom("ok"));
				break;
			case "set_speed_limit": {
				OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
				double speedlimit = ((OtpErlangDouble) args.elementAt(0))
						.doubleValue();
				if (speedlimit < 0) {
					this.simulation.setSpeedLimit(null);
				} else {
					this.simulation.setSpeedLimit(speedlimit);
				}
				this.conn.send(sender, new OtpErlangAtom("ok"));
				break;
			}
			case "set_random_seed": {
				OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
				long seed = ((OtpErlangLong) args.elementAt(0)).longValue();
				this.simulation.setRandomSeed(seed);
				this.conn.send(sender, new OtpErlangAtom("ok"));
				break;
			}
			case "is_running":
				this.conn.send(sender,
						new OtpErlangAtom(this.simulation.isRunning()));
				break;
			case "simulation_time":
				this.conn.send(sender,
						new OtpErlangLong(this.simulation.getSimulationTime()));
				break;
			case "simulation_time_ms":
				this.conn.send(
						sender,
						new OtpErlangLong(this.simulation
								.getSimulationTimeMillis()));
				break;
			case "simulation_step_ms":
				if (this.simulation.isRunning()) {
					this.conn.send(sender, new OtpErlangAtom("running"));
				} else {
					simulation.stepMillisecondSimulation();
					while (simulation.isRunning()) {
						sleep(1);
					}
					this.conn.send(sender, new OtpErlangAtom("ok"));
				}
				break;
			case "simulation_step": {
				if (this.simulation.isRunning()) {
					this.conn.send(sender, new OtpErlangAtom("running"));
				} else {
					OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
					long time = ((OtpErlangLong) args.elementAt(0)).longValue();
					TimeEvent stopEvent = new TimeEvent(0) {
						@Override
						public void execute(long arg0) {
							simulation.stopSimulation();
						}
					};
					simulation.scheduleEvent(stopEvent,
							simulation.getSimulationTime()
									+ (time * Simulation.MILLISECOND));
					simulation.startSimulation();
					while (simulation.isRunning()) {
						sleep(100);
					}
					this.conn.send(sender, new OtpErlangAtom("ok"));
				}
				break;
			}
			case "radio_set_config": {
				OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
				String radio = ((OtpErlangString) ((OtpErlangTuple) args)
						.elementAt(0)).stringValue();
				if (!(simulation.getRadioMedium().getClass().toString().equals(radio))) {
					this.conn.send(sender, new OtpErlangAtom(
							"radio_medium_nomatch"));
					break;
				}
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
					} catch (Exception e) {
						this.conn.send(sender, new OtpErlangAtom(
								"radio_medium_badoptions"));
						break;
					}
				} else {
					/* fallback */
					this.conn.send(sender, new OtpErlangAtom(
							"radio_medium_unsupported"));
					break;
				}
				this.conn.send(sender, new OtpErlangAtom("ok"));
			}
			case "radio_get_config": {
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
				this.conn.send(sender, new OtpErlangTuple(retval));
				break;
			}
			/*
			 * Mote specific calls
			 */
			case "mote_types": {
				MoteType[] types = simulation.getMoteTypes();
				OtpErlangObject[] retval = new OtpErlangObject[types.length];
				for (int i = 0; i < types.length; i++) {
					retval[i] = new OtpErlangList(types[i].getIdentifier());
				}
				this.conn.send(sender, new OtpErlangList(retval));
				break;
			}
			case "mote_add": {
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
					this.conn.send(sender, new OtpErlangTuple(retval));
				} else {
					this.conn.send(sender, new OtpErlangAtom("unknown_type"));
				}
				break;
			}
			case "mote_del": {
				int id;
				try {
					OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
					id = ((OtpErlangInt) args.elementAt(0)).intValue();
				} catch (OtpErlangRangeException e) {
					this.conn.send(sender, new OtpErlangAtom("badid"));
					break;
				}
				try {
					Mote m = simulation.getMoteWithID(id);
					simulation.removeMote(m);
					this.conn.send(sender, new OtpErlangAtom("ok"));
				} catch (Exception e) {
					this.conn.send(sender, new OtpErlangAtom("fail"));
				}
				break;
			}
			case "mote_get_pos": {
				int id;
				try {
					OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
					id = ((OtpErlangLong) args.elementAt(0)).intValue();
				} catch (OtpErlangRangeException e) {
					this.conn.send(sender, new OtpErlangAtom("badid"));
					break;
				}
				try {
					Mote m = simulation.getMoteWithID(id);
					double x = m.getInterfaces().getPosition().getXCoordinate();
					double y = m.getInterfaces().getPosition().getYCoordinate();
					double z = m.getInterfaces().getPosition().getZCoordinate();
					OtpErlangObject[] retval = new OtpErlangObject[3];
					retval[0] = new OtpErlangDouble(x);
					retval[1] = new OtpErlangDouble(y);
					retval[2] = new OtpErlangDouble(z);
					this.conn.send(sender, new OtpErlangTuple(retval));
				} catch (Exception e) {
					this.conn.send(sender, new OtpErlangAtom("fail"));
				}
				break;
			}
			case "mote_set_pos": {
				int id;
				double x, y, z;
				try {
					OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
					id = ((OtpErlangLong) args.elementAt(0)).intValue();
					x = ((OtpErlangDouble) args.elementAt(1)).doubleValue();
					y = ((OtpErlangDouble) args.elementAt(2)).doubleValue();
					z = ((OtpErlangDouble) args.elementAt(3)).doubleValue();
				} catch (OtpErlangRangeException e) {
					this.conn.send(sender, new OtpErlangAtom("badid"));
					break;
				}
				try {
					Mote m = simulation.getMoteWithID(id);
					m.getInterfaces().getPosition().setCoordinates(x, y, z);
					this.conn.send(sender, new OtpErlangAtom("ok"));
				} catch (Exception e) {
					this.conn.send(sender, new OtpErlangAtom("fail"));
				}
				break;
			}
			case "motes": {
				Mote[] motes = this.simulation.getMotes();
				OtpErlangObject[] elements = new OtpErlangObject[motes.length];
				for (int i = 0; i < motes.length; i++) {
					elements[i] = new OtpErlangInt(motes[i].getID());
				}
				this.conn.send(sender, new OtpErlangList(elements));
				break;
			}
			case "mote_write": {
				int id;
				String data;
				try {
					OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
					id = ((OtpErlangLong) args.elementAt(0)).intValue();
					data = ((OtpErlangString) args.elementAt(1)).stringValue();
				} catch (OtpErlangRangeException e) {
					this.conn.send(sender, new OtpErlangAtom("badid"));
					break;
				} catch (Exception e) {
					this.conn.send(sender, new OtpErlangAtom("badargs"));
					break;
				}
				Mote mote = this.simulation.getMoteWithID(id);
				SerialPort serialPort = (SerialPort) mote.getInterfaces()
						.getLog();
				serialPort.writeString(data);
				this.conn.send(sender, new OtpErlangAtom("ok"));
				break;
			}
			case "mote_listen": {
				int id;
				try {
					OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
					id = ((OtpErlangLong) args.elementAt(0)).intValue();
				} catch (OtpErlangRangeException e) {
					this.conn.send(sender, new OtpErlangAtom("badid"));
					break;
				}
				if (!motes_observer.containsKey(id)) {
					Mote mote = this.simulation.getMoteWithID(id);
					SerialPort serial_port = (SerialPort) mote.getInterfaces()
							.getLog();
					SerialObserver obs = new SerialObserver(motes_output,
							serial_port, id);
					motes_output.put(id, "");
					motes_observer.put(id, obs);
					serial_port.addSerialDataObserver(obs);
					this.conn.send(sender, new OtpErlangAtom("ok"));
				} else {
					this.conn.send(sender, new OtpErlangAtom(
							"already_listened_on"));
				}
				break;
			}
			case "mote_hw_listen": {
				int id;
				try {
					OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
					id = ((OtpErlangLong) args.elementAt(0)).intValue();
				} catch (OtpErlangRangeException e) {
					this.conn.send(sender, new OtpErlangAtom("badid"));
					break;
				}
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
					this.conn.send(sender, new OtpErlangAtom("ok"));
				} else {
					this.conn.send(sender, new OtpErlangAtom(
							"already_listened_on"));
				}
				break;
			}
			case "mote_unlisten": {
				int id;
				try {
					OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
					id = ((OtpErlangLong) args.elementAt(0)).intValue();
				} catch (OtpErlangRangeException e) {
					this.conn.send(sender, new OtpErlangAtom("badid"));
					break;
				}
				if (motes_observer.containsKey(id)) {
					Observer obs = this.motes_observer.get(id);
					Mote mote = this.simulation.getMoteWithID(id);
					SerialPort serial_port = (SerialPort) mote.getInterfaces()
							.getLog();
					serial_port.deleteSerialDataObserver(obs);
					motes_observer.remove(id);
					motes_output.remove(id);
					this.conn.send(sender, new OtpErlangAtom("ok"));
				} else {
					this.conn
							.send(sender, new OtpErlangAtom("not_listened_on"));
				}
				break;
			}
			case "mote_hw_unlisten": {
				int id;
				try {
					OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
					id = ((OtpErlangLong) args.elementAt(0)).intValue();
				} catch (OtpErlangRangeException e) {
					this.conn.send(sender, new OtpErlangAtom("badid"));
					break;
				}
				if (motes_hw_observer.containsKey(id)) {
					Observer obs = this.motes_hw_observer.get(id);
					Mote mote = this.simulation.getMoteWithID(id);
					LED led = (LED) mote.getInterfaces().getLED();
					Radio radio = (Radio) mote.getInterfaces().getRadio();
					led.deleteObserver(obs);
					radio.deleteObserver(obs);
					motes_hw_observer.remove(id);
					motes_hw_events.remove(id);
					this.conn.send(sender, new OtpErlangAtom("ok"));
				} else {
					this.conn
							.send(sender, new OtpErlangAtom("not_listened_on"));
				}
				break;
			}
			case "mote_hw_events": {
				int id;
				try {
					OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
					id = ((OtpErlangLong) args.elementAt(0)).intValue();
				} catch (OtpErlangRangeException e) {
					this.conn.send(sender, new OtpErlangAtom("badid"));
					break;
				}
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
						elements[i] = (OtpErlangObject) (new OtpErlangTuple(
								element));
					}
					this.conn.send(sender, new OtpErlangList(elements));
				} else {
					this.conn
							.send(sender, new OtpErlangAtom("not_listened_on"));
				}
				break;
			}
			case "mote_read_pushback": {
				int id;
				String data;
				String old_data;
				String new_data;
				try {
					OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
					id = ((OtpErlangLong) args.elementAt(0)).intValue();
					if (((OtpErlangList) args.elementAt(1)).arity() > 0) {
						data = ((OtpErlangString) args.elementAt(1))
								.stringValue();
					} else {
						data = "";
					}
				} catch (OtpErlangRangeException e) {
					this.conn.send(sender, new OtpErlangAtom("badid"));
					break;
				} catch (Exception e) {
					this.conn.send(sender, new OtpErlangAtom("badargs"));
					break;
				}
				do {
					old_data = motes_output.get(id);
					if (old_data == null) {
						new_data = data;
					} else {
						new_data = data + old_data;
					}
				} while (!motes_output.replace(id, old_data, new_data));
				this.conn.send(sender, new OtpErlangAtom("ok"));
				break;
			}
			case "mote_read": {
				h_mote_read(sender, msg);
				break;
			}
			case "mote_read_s": {
				h_mote_read(sender, msg);
				this.simulation.stopSimulation();
				break;
			}
			case "msg_wait": {
				String data;
				OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
				data = ((OtpErlangString) args.elementAt(0)).stringValue();
				wait_for_msg(data);
				this.conn.send(sender, new OtpErlangAtom("ok"));
				break;
			}
			case "get_last_event": {
				int id;
				OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
				try {
					id = ((OtpErlangLong) args.elementAt(0)).intValue();
				} catch (OtpErlangRangeException e) {
					this.conn.send(sender, new OtpErlangAtom("badid"));
					break;
				}
				String data = motes_output.get(id);
				if (data == null) {
					this.conn
							.send(sender, new OtpErlangAtom("not_listened_to"));
					return;
				} else {
					Pattern regex = Pattern.compile("(EVENT:[^\n]*\n)");
					Matcher m = regex.matcher(data);
					StringBuffer result = new StringBuffer();
					if (m.find()) {
						m.appendReplacement(result, "");
						m.appendTail(result);
						if (motes_output.replace(id, data, result.toString())) {
							this.conn.send(sender,
									new OtpErlangList(m.group(1)));
						} else {
							this.conn
									.send(sender, new OtpErlangAtom("updated"));
						}
					} else {
						this.conn.send(sender, new OtpErlangAtom("no_event"));
					}
					break;
				}
			}
			/*
			 * Default
			 */
			default:
				logger.fatal("Undefined message\n");
				this.conn.send(sender, new OtpErlangAtom("undef"));
				break;
			}
		} catch (IOException e) {
			try {
				this.conn.send(sender, new OtpErlangAtom("error"));
			} catch (IOException e1) {
				logger.fatal(e1.getMessage());
				System.exit(1);
			}
			logger.fatal(e.getMessage());
			System.exit(1);
		}
	}

	private void h_mote_read(OtpErlangPid sender, OtpErlangTuple msg)
			throws IOException {
		int id;
		try {
			OtpErlangTuple args = ((OtpErlangTuple) msg.elementAt(2));
			id = ((OtpErlangLong) args.elementAt(0)).intValue();
		} catch (OtpErlangRangeException e) {
			this.conn.send(sender, new OtpErlangAtom("badid"));
			return;
		}
		String ret = motes_output.replace(id, "");
		if (ret == null) {
			this.conn.send(sender, new OtpErlangAtom("not_listened_to"));
		} else {
			this.conn.send(sender, new OtpErlangList(ret));
		}
	}
}
