package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;

import sim.Simulation;
import utils.Parser;
import utils.TraceListener;

public class SimpleView extends JPanel {
	
	JPanel p1 = new JPanel();
	JTextArea t1 = new JTextArea();
	JTextArea t2 = new JTextArea();

	public static void main(String[] args){
		// make a frame for the GUI
		FullFrame frame = new FullFrame("SimpleView");

		// make a clock view and add to the frame
		ClockView clockView = new ClockView();
		frame.add(clockView,BorderLayout.NORTH);
		
		// Make a control and report panel and add to the frame
		JPanel reportPanel = new JPanel();
		reportPanel.setLayout(new BorderLayout());

		TabView tab = new TabView();
		StartView startPanel = new StartView();
		
		reportPanel.add(tab, BorderLayout.CENTER);
		reportPanel.add(startPanel, BorderLayout.NORTH);

		frame.add(reportPanel,BorderLayout.CENTER);
		
		CommandView command = new CommandView();
		frame.add(command,BorderLayout.SOUTH);
		
		// make the frame visible etc
		frame.setVisible(true);
		frame.pack();
		frame.validate();

		// instantiate a simulation and link the GUI components
		Simulation mySimulation = new Simulation(false);
		mySimulation.initialise(args);

		mySimulation.addClockListener(clockView);
		clockView.addActionListener(mySimulation.getGameClock().getClockController());
		startPanel.setSimulation(mySimulation);
		command.setSimulation(mySimulation);
	}

	public SimpleView(){
		this.setLayout(new BorderLayout());
	}

}
