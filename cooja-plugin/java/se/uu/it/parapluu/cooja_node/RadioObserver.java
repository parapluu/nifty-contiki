package se.uu.it.parapluu.cooja_node;

import java.util.LinkedList;
import java.util.Observable;
import java.util.Observer;
import org.contikios.cooja.RadioConnection;
import org.contikios.cooja.RadioMedium;
import org.contikios.cooja.RadioPacket;
import org.contikios.cooja.Simulation;

import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class RadioObserver implements Observer {
	private LinkedList<OtpErlangObject> messages;
	private RadioMedium radioMedium;
	private Simulation simulation;
	
	
	
	public RadioObserver(Simulation simulation,
			LinkedList<OtpErlangObject> messages,
			RadioMedium radioMedium) {
		super();
		this.simulation = simulation;
		this.setMessages(messages);
		this.radioMedium = radioMedium;
	}



	@Override
	public void update(Observable obs, Object obj) {
		RadioConnection conn = radioMedium.getLastConnection();
	    if (conn == null) {
	        return;
	    }

	    long startTime = conn.getStartTime();
	    long endTime = simulation.getSimulationTime();
	    RadioPacket packet = conn.getSource().getLastPacketTransmitted();
	    byte[] packet_data = packet.getPacketData();
	    int id = conn.getSource().getMote().getID();
	    
	    OtpErlangObject msg[] = new OtpErlangObject[4];
	    msg[0] = new OtpErlangInt(id);
	    msg[1] = new OtpErlangList(packet_data.toString());
	    msg[2] = new OtpErlangLong(startTime);
	    msg[3] = new OtpErlangLong(endTime);

	    messages.addLast(new OtpErlangTuple(msg));
	}



	public LinkedList<OtpErlangObject> getMessages() {
		return messages;
	}

	public void setMessages(LinkedList<OtpErlangObject> messages) {
		this.messages = messages;
	}

}
