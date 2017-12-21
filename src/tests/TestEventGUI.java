package tests;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;

import utils.Tracer;
import view.ClockView;
import view.CommandView;
import view.FullFrame;
import view.TraceView;

public class TestEventGUI {
	
	private TestEvent testEvent; 
	private static String label = "Test";
	private static String testName = "TestMoveEvent";

	public static void main(String[] args){
		TestEventGUI test = new TestEventGUI(label, testName, args);
	}

	public TestEventGUI(String label, String testName, String[] args) {
		testEvent = new TestEvent(args);
		makeFrame(label);
	}

	protected void makeFrame(String label){
		CommandView commandView = new CommandView();
		commandView.setInterpreter(testEvent.getInterpreter());
		ClockView clockView = new ClockView();
		testEvent.getScenario().addClockListener(clockView);
		clockView.addActionListener(testEvent.getScenario().getClockController());
		FullFrame frame = new FullFrame(label);
		TraceView traceView = new TraceView();
		Tracer.addListener(traceView);
		frame.add(traceView,BorderLayout.CENTER);
		JButton startBtn = new JButton("Start");
		startBtn.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				Tracer.write("hit start");
				Runnable runnable = new RunSim();
				Thread thread = new Thread(runnable);
				thread.start();
			}
		});
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
		
		public RunSim(){
		}

		@Override
		public void run() {
			Tracer.write("running");
//			testEvent.runTest();
			testEvent.test(testName);
		}
		
	}
}
