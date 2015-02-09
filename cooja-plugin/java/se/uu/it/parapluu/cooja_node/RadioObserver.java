package se.uu.it.parapluu.cooja_node;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Observable;
import java.util.Observer;

import org.contikios.cooja.ConvertedRadioPacket;
import org.contikios.cooja.RadioConnection;
import org.contikios.cooja.RadioMedium;
import org.contikios.cooja.RadioPacket;
import org.contikios.cooja.Simulation;
import org.contikios.cooja.interfaces.Radio;

import se.uu.it.parapluu.cooja_node.analyzers.FragHeadPacketAnalyzer;
import se.uu.it.parapluu.cooja_node.analyzers.ICMPv6Analyzer;
import se.uu.it.parapluu.cooja_node.analyzers.IEEE802154Analyzer;
import se.uu.it.parapluu.cooja_node.analyzers.IPHCPacketAnalyzer;
import se.uu.it.parapluu.cooja_node.analyzers.IPv6PacketAnalyzer;
import se.uu.it.parapluu.cooja_node.analyzers.PacketAnalyzer;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class RadioObserver implements Observer {
	private LinkedList<OtpErlangObject> messages;
	private RadioMedium radioMedium;
	private ArrayList<PacketAnalyzer> analyzers = null;

	private ArrayList<PacketAnalyzer> lowpanAnalyzers = null;
	private ArrayList<PacketAnalyzer> lowpanAnalyzersPcap = null;

	public RadioObserver(Simulation simulation,
			LinkedList<OtpErlangObject> messages, RadioMedium radioMedium,
			int analyzer) {
		super();
		this.setMessages(messages);
		this.radioMedium = radioMedium;

		lowpanAnalyzers = new ArrayList<PacketAnalyzer>();
		lowpanAnalyzers.add(new IEEE802154Analyzer(false));
		lowpanAnalyzers.add(new FragHeadPacketAnalyzer());
		lowpanAnalyzers.add(new IPHCPacketAnalyzer());
		lowpanAnalyzers.add(new IPv6PacketAnalyzer());
		lowpanAnalyzers.add(new ICMPv6Analyzer());

		lowpanAnalyzersPcap = new ArrayList<PacketAnalyzer>();
		lowpanAnalyzersPcap.add(new IEEE802154Analyzer(true));
		lowpanAnalyzersPcap.add(new FragHeadPacketAnalyzer());
		lowpanAnalyzersPcap.add(new IPHCPacketAnalyzer());
		lowpanAnalyzersPcap.add(new IPv6PacketAnalyzer());
		lowpanAnalyzersPcap.add(new ICMPv6Analyzer());

		switch (analyzer) {
		case 0:
			this.analyzers = null;
			break;
		case 1:
			this.analyzers = lowpanAnalyzers;
			break;
		case 2:
			this.analyzers = lowpanAnalyzersPcap;
			break;
		default:
			this.analyzers = null;
		}
	}

	@Override
	public void update(Observable obs, Object obj) {
		RadioConnection conn = radioMedium.getLastConnection();
		if (conn == null) {
			return;
		}
		messages.addLast(decode_packet(conn));
	}

	private OtpErlangObject decode_packet(RadioConnection conn) {
		byte[] data;
		RadioPacket radio_packet = conn.getSource().getLastPacketTransmitted();

		if (radio_packet == null) {
			data = null;
		} else if (radio_packet instanceof ConvertedRadioPacket) {
			data = ((ConvertedRadioPacket) radio_packet)
					.getOriginalPacketData();
		} else {
			data = radio_packet.getPacketData();
		}
		if (data == null) {
			data = "[unknown data]".getBytes();
		}

		LinkedList<OtpErlangObject> analysis = new LinkedList<OtpErlangObject>();

		/* default analyzer */
		PacketAnalyzer.Packet packet = new PacketAnalyzer.Packet(data,
				PacketAnalyzer.MAC_LEVEL);

		if (analyzePacket(packet, analysis)) {
			if (packet.hasMoreData()) {
				byte[] payload = packet.getPayload();
				OtpErlangObject tpl_payload[] = new OtpErlangObject[2];
				tpl_payload[0] = new OtpErlangAtom("payload");
				tpl_payload[1] = new OtpErlangBinary(payload);
				analysis.add(new OtpErlangTuple(tpl_payload));
			}
		} else {
			OtpErlangObject tpl_payload[] = new OtpErlangObject[2];
			tpl_payload[0] = new OtpErlangAtom("payload");
			tpl_payload[1] = new OtpErlangBinary(data);
			analysis.add(new OtpErlangTuple(tpl_payload));
		}

		OtpErlangObject[] msg = new OtpErlangObject[3];

		OtpErlangInt source = new OtpErlangInt(conn.getSource().getMote()
				.getID());
		Radio[] dest_radios = conn.getDestinations();
		OtpErlangObject[] destination = new OtpErlangObject[dest_radios.length];
		for (int i = 0; i < dest_radios.length; i++) {
			destination[i] = new OtpErlangInt(dest_radios[i].getMote().getID());
		}

		msg[0] = source;
		msg[1] = new OtpErlangList(destination);

		OtpErlangObject decoded_packet[] = new OtpErlangObject[analysis.size()];
		int i = 0;
		for (OtpErlangObject e : analysis) {
			decoded_packet[i++] = e;
		}
		msg[2] = new OtpErlangList(decoded_packet);

		return new OtpErlangTuple(msg);
	}

	private boolean analyzePacket(PacketAnalyzer.Packet packet,
			LinkedList<OtpErlangObject> analysis) {
		if (analyzers == null)
			return false;
		try {
			boolean analyze = true;
			while (analyze) {
				analyze = false;
				for (int i = 0; i < analyzers.size(); i++) {
					PacketAnalyzer analyzer = analyzers.get(i);
					if (analyzer.matchPacket(packet)) {
						int res = analyzer.analyzePacket(packet, analysis);
						if (res != PacketAnalyzer.ANALYSIS_OK_CONTINUE) {
							/*
							 * this was the final or the analysis failed - no
							 * analyzable payload possible here...
							 */
							return analysis.size() > 0;
						}
						/* continue another round if more bytes left */
						analyze = packet.hasMoreData();
						break;
					}
				}
			}
		} catch (Exception e) {
			return false;
		}
		return analysis.size() > 0;
	}

	public LinkedList<OtpErlangObject> getMessages() {
		return messages;
	}

	public void setMessages(LinkedList<OtpErlangObject> messages) {
		this.messages = messages;
	}

}
