package se.uu.it.parapluu.cooja_node.analyzers;

import java.util.LinkedList;

import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ICMPv6Analyzer extends PacketAnalyzer {

	public static final byte ICMPv6_DISPATCH = 58;

	public static final int ECHO_REQUEST = 128;
	public static final int ECHO_REPLY = 129;
	public static final int GROUP_QUERY = 130;
	public static final int GROUP_REPORT = 131;
	public static final int GROUP_REDUCTION = 132;
	public static final int ROUTER_SOLICITATION = 133;
	public static final int ROUTER_ADVERTISEMENT = 134;
	public static final int NEIGHBOR_SOLICITATION = 135;
	public static final int NEIGHBOR_ADVERTISEMENT = 136;

	public static final int RPL_CODE_DIS = 0; /* DIS message */
	public static final int RPL_CODE_DIO = 1; /* DIO message */
	public static final int RPL_CODE_DAO = 2;/* DAO message */
	public static final int RPL_CODE_DAO_ACK = 3;/* DAO ACK message */

	public static final int FLAG_ROUTER = 0x80;
	public static final int FLAG_SOLICITED = 0x40;
	public static final int FLAG_OVERRIDE = 0x20;

	public static final int ON_LINK = 0x80;
	public static final int AUTOCONFIG = 0x40;

	public static final int SOURCE_LINKADDR = 1;
	public static final int TARGET_LINKADDR = 2;
	public static final int PREFIX_INFO = 3;
	public static final int MTU_INFO = 5;

	public static final String[] TYPE_NAME = new String[] { "Echo Request",
			"Echo Reply", "Group Query", "Group Report", "Group Reduction",
			"Router Solicitation", "Router Advertisement",
			"Neighbor Solicitation", "Neighbor Advertisement", "Redirect",
			"Router Renumber", "Node Information Query",
			"Node Information Response" };

	public static final String[] BRIEF_TYPE_NAME = new String[] { "ECHO REQ",
			"ECHO RPLY", "GRP QUERY", "GRP REPORT", "GRP REDUCTION", "RS",
			"RA", "NS", "NA", "REDIRECT", "ROUTER RENUMBER", "NODE INFO QUERY",
			"NODE INFO RESP" };

	@Override
	public int analyzePacket(Packet packet, LinkedList<OtpErlangObject> analysis) {
		int type = packet.get(0) & 0xff;
		int code = packet.get(1) & 0xff;
		// int checksum = ((packet.get(2) & 0xff) << 8) | packet.get(3) & 0xff;

		OtpErlangList opts = null;

		if (type >= 128 && (type - 128) < TYPE_NAME.length) {
			OtpErlangTuple etype = PacketAnalyzer.make_opt(
					"type",
					new OtpErlangString(BRIEF_TYPE_NAME[type - 128]
							.toLowerCase()));
			OtpErlangTuple ecode = PacketAnalyzer.make_opt(
					"code", new OtpErlangInt(code));
			OtpErlangObject[] objs = { etype, ecode };
			opts = new OtpErlangList(objs);
		} else if (type == 155) {
			/* RPL */
			switch (code) {
			case RPL_CODE_DIS: {
				OtpErlangTuple etype = PacketAnalyzer.make_opt(
						"type", new OtpErlangString(
								"RPL DIS"));
				OtpErlangTuple ecode = PacketAnalyzer.make_opt(
						"code", new OtpErlangInt(code));
				OtpErlangObject[] objs = { etype, ecode };
				opts = new OtpErlangList(objs);
				break;
			}
			case RPL_CODE_DIO: {
				OtpErlangTuple etype = PacketAnalyzer.make_opt(
						"type", new OtpErlangString(
								"RPL DIO"));
				OtpErlangTuple ecode = PacketAnalyzer.make_opt(
						"code", new OtpErlangInt(code));
				int instanceID = packet.get(4) & 0xff;
				int version = packet.get(5) & 0xff;
				int rank = ((packet.get(6) & 0xff) << 8)
						+ (packet.get(7) & 0xff);
				int mop = (packet.get(8) >> 3) & 0x07;
				int dtsn = packet.get(9) & 0xFF;

				OtpErlangTuple einstanceid = PacketAnalyzer.make_opt(
						"instanceid", new OtpErlangInt(
								instanceID));
				OtpErlangTuple eversion = PacketAnalyzer
						.make_opt("version",
								new OtpErlangInt(version));
				OtpErlangTuple erank = PacketAnalyzer.make_opt(
						"rank", new OtpErlangInt(rank));
				OtpErlangTuple emop = PacketAnalyzer.make_opt(
						"mop", new OtpErlangInt(mop));
				OtpErlangTuple edtsn = PacketAnalyzer.make_opt(
						"dtsn", new OtpErlangInt(dtsn));

				OtpErlangObject[] objs = { etype, ecode, einstanceid, eversion,
						erank, emop, edtsn };
				opts = new OtpErlangList(objs);
				packet.consumeBytesStart(8);
				break;
			}
			case RPL_CODE_DAO: {
				OtpErlangTuple etype = PacketAnalyzer.make_opt(
						"type", new OtpErlangString(
								"RPL DAO"));
				OtpErlangTuple ecode = PacketAnalyzer.make_opt(
						"code", new OtpErlangInt(code));
				OtpErlangObject[] objs = { etype, ecode };
				opts = new OtpErlangList(objs);
				/* TODO: add DAO fields */
				break;
			}
			case RPL_CODE_DAO_ACK: {
				OtpErlangTuple etype = PacketAnalyzer.make_opt(
						"type", new OtpErlangString(
								"RPL DAO ACK"));
				OtpErlangTuple ecode = PacketAnalyzer.make_opt(
						"code", new OtpErlangInt(code));
				OtpErlangObject[] objs = { etype, ecode };
				opts = new OtpErlangList(objs);
				break;
			}
			default: {
				OtpErlangTuple etype = PacketAnalyzer.make_opt(
						"type", new OtpErlangString(
								"RPL UNKNOWN"));
				OtpErlangTuple ecode = PacketAnalyzer.make_opt(
						"code", new OtpErlangInt(code));
				OtpErlangObject[] objs = { etype, ecode };
				opts = new OtpErlangList(objs);
			}
			}
		}
		analysis.add(PacketAnalyzer.make_opt("icmpv6", opts));
		/* remove type, code, crc */
		packet.consumeBytesStart(4);
		return ANALYSIS_OK_FINAL;
	}

	@Override
	public boolean matchPacket(Packet packet) {
		return packet.level == APPLICATION_LEVEL
				&& packet.lastDispatch == ICMPv6_DISPATCH;
	}
}
