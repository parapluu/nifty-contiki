package se.uu.it.parapluu.cooja_node.analyzers;

import java.util.LinkedList;

import org.contikios.cooja.util.IPUtils;

import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class IPv6PacketAnalyzer extends PacketAnalyzer {

	public final static int PROTO_UDP = 17;
	public final static int PROTO_TCP = 6;
	public final static int PROTO_ICMP = 58;

	public final static byte[] UNSPECIFIED_ADDRESS = { 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0 };

	private static final int IPV6_DISPATCH = 0x41;

	@Override
	public boolean matchPacket(Packet packet) {
		return packet.level == NETWORK_LEVEL && packet.get(0) == IPV6_DISPATCH;
	}

	@Override
	public int analyzePacket(Packet packet, LinkedList<OtpErlangObject> analysis) {

		/* if packet has less than 40 bytes it is not interesting ... */
		if (packet.size() < 40)
			return ANALYSIS_FAILED;

		int pos = 1;

		int trafficClass = 0;
		int flowLabel = 0;
		packet.getInt(pos + 4, 2);
		int proto = packet.getInt(pos + 6, 1);
		packet.getInt(pos + 7, 1);
		byte[] srcAddress = new byte[16];
		byte[] destAddress = new byte[16];

		packet.copy(pos + 8, srcAddress, 0, 16);
		packet.copy(pos + 24, destAddress, 0, 16);

		String protoStr = "" + proto;
		if (proto == PROTO_ICMP) {
			protoStr = "ICMPv6";
		} else if (proto == PROTO_UDP) {
			protoStr = "UDP";
		} else if (proto == PROTO_TCP) {
			protoStr = "TCP";
		}

		/* consume dispatch + IP header */
		packet.pos += 41;

		StringBuilder from_addr = new StringBuilder();
		IPUtils.getUncompressedIPv6AddressString(from_addr, srcAddress);
		StringBuilder to_addr = new StringBuilder();
		IPUtils.getUncompressedIPv6AddressString(to_addr, destAddress);

		OtpErlangTuple protocol = PacketAnalyzer.make_opt(
				"protocol", new OtpErlangList(protoStr));
		OtpErlangTuple tc = PacketAnalyzer.make_opt("tc",
				new OtpErlangInt(trafficClass));
		OtpErlangTuple fl = PacketAnalyzer.make_opt("fl",
				new OtpErlangInt(flowLabel));
		OtpErlangTuple from = PacketAnalyzer.make_opt(
				"from",
				new OtpErlangList(from_addr.toString()));
		OtpErlangTuple to = PacketAnalyzer.make_opt("to",
				new OtpErlangList(to_addr.toString()));
		OtpErlangObject[] objs = { protocol, tc, fl, from, to };

		analysis.add(PacketAnalyzer.make_opt("ipv6",
				new OtpErlangList(objs)));

		packet.lastDispatch = (byte) (proto & 0xff);
		packet.level = APPLICATION_LEVEL;
		return ANALYSIS_OK_CONTINUE;
	}
}
