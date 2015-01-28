/*  -------------------------------------------------------------------
	Copyright (c) 2014, Andreas Löscher <andreas.loscher@it.uu.se> and
 	All rights reserved.

 	This file is distributed under the Simplified BSD License.
 	Details can be found in the LICENSE file.
    ------------------------------------------------------------------- */

package se.uu.it.parapluu.cooja_node;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JLabel;

//import org.apache.log4j.Logger;
import org.contikios.cooja.ClassDescription;
import org.contikios.cooja.Cooja;
import org.contikios.cooja.PluginType;
import org.contikios.cooja.Simulation;
import org.contikios.cooja.VisPlugin;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

@SuppressWarnings("serial")
@ClassDescription("Nifty Control Plugin")
@PluginType(PluginType.SIM_CONTROL_PLUGIN)
public class SocketControlPlugin extends VisPlugin {
	//private static Logger logger = Logger.getLogger(SocketControlPlugin.class);
	private final static int LABEL_WIDTH = 100;
	private final static int LABEL_HEIGHT = 15;
	private final long MAGIC_NUMBER = 0L;
	private JLabel label;
	
	
	private OtpSelf self;


	private String name = "cooja_control";
	private String peer = "cooja_master";

	private OtpPeer enode;

	private OtpConnection conn;
	private MessageHandler handler;

	public SocketControlPlugin(Simulation sim, Cooja gui) {
		super("Nifty Control Plugin", gui, false);

		if (Cooja.isVisualized()) {
			Box northBox = Box.createHorizontalBox();
			northBox.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

			Box mainBox = Box.createHorizontalBox();
			mainBox.setBorder(BorderFactory.createEmptyBorder(0, 5, 5, 5));

			label = new JLabel("Nifty Control Plugin: connected");
			label.setPreferredSize(new Dimension(LABEL_WIDTH, LABEL_HEIGHT));
			mainBox.add(BorderLayout.CENTER, label);

			getContentPane().add(BorderLayout.NORTH, northBox);
			getContentPane().add(BorderLayout.CENTER, mainBox);
			pack();
		}
	
		try {
			connect();
			this.handler = new MessageHandler(conn, self.createPid(), sim);
			this.handler.start();
			sim.stopSimulation();
			sim.setRandomSeed(this.MAGIC_NUMBER);
		} catch (IOException | OtpAuthException e) {
			if (Cooja.isVisualized()) {
				label.setText("Nifty Control Plugin: not connected");
			}
		}
	}

	private void connect() throws IOException, OtpAuthException {
		this.self = new OtpSelf(this.name);
		this.enode = new OtpPeer(this.peer);
		this.conn = this.self.connect(this.enode);
	}

	public void closePlugin() {
		if (this.conn!= null) {
			this.conn.close();
		}
	}
}
