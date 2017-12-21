package tests;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;

import interpreter.CommandInterpreter;
import models.EventQueue;
import sim.Scenario;
import utils.Tracer;
import view.ClockView;
import view.CommandView;
import view.FullFrame;
import view.TraceView;

public class TestView extends JFrame{
	public TestView(String label, CommandInterpreter interpreter, Scenario scenario, ActionListener listener ){
		CommandView commandView = new CommandView();
		commandView.setInterpreter(interpreter);
		ClockView clockView = new ClockView();
		scenario.addClockListener(clockView);
		clockView.addActionListener(scenario.getClockController());
		FullFrame frame = new FullFrame(label);
		TraceView traceView = new TraceView();
		Tracer.addListener(traceView);
		frame.add(traceView,BorderLayout.CENTER);
		JButton startBtn = new JButton("Start");
		startBtn.addActionListener(listener);
		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());
		panel.add(startBtn, BorderLayout.WEST);
		panel.add(commandView,BorderLayout.CENTER);
		panel.add(clockView, BorderLayout.SOUTH);
		frame.add(panel,BorderLayout.NORTH);
//		frame.add(view.makeControlPanel(),BorderLayout.SOUTH);
		frame.setVisible(true);
		frame.pack();
		frame.validate();
	}
	
	class RunSim implements Runnable{
		
		EventQueue queue;
		Scenario scenario;
		
		public RunSim(EventQueue q, Scenario s){
			queue = q;
			scenario = s;
		}

		@Override
		public void run() {
			while (scenario.getClock() < scenario.getParameters().getEndTime()){
				queue.doEvents(scenario.getClock());
				scenario.incrementClock(1.0);
			}
		}
	}


}
