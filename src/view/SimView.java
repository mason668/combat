package view;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

import data.CSData;
import interpreter.CommandInterpreter;
import sim.Scenario;
import sim.Simulation;
import tests.TestEvent;
import utils.Logger;
import utils.Tracer;
import view.ClockView;
import view.CommandView;
import view.FullFrame;
import view.TraceView;

public class SimView {
	
	private static String label = "Simulation";
	private static String testName = "TestMoveEvent";
	private Scenario myScenario = new Scenario("sim gui", false);
	private CSData myData = new CSData();
	private CommandInterpreter myInterpreter = new CommandInterpreter(myData, myScenario);
	
	private MapView mapView = new MapView();

	public static void main(String[] args){
		SimView test = new SimView(label, testName, args);
	}

	public SimView(String label, String testName, String[] args) {
		makeFrame(label);
		if (args.length<=0){
			myInterpreter.interpret("load init.txt");
		} else {
			myInterpreter.interpret(args);
		}
		mapView.setMap(myScenario.getMap());
	}

	protected void makeFrame(String label){
		FullFrame frame = new FullFrame(label);

		JPanel controlPanel = new JPanel();
		controlPanel.setLayout(new BorderLayout());

		CommandView commandView = new CommandView();
		commandView.setInterpreter(myInterpreter);
		controlPanel.add(commandView,BorderLayout.CENTER);

		ClockView clockView = new ClockView();
		myScenario.addClockListener(clockView);
		clockView.addActionListener(myScenario.getClockController());
		controlPanel.add(clockView, BorderLayout.SOUTH);
		
		JButton startBtn = new JButton("Start");
		startBtn.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				Tracer.write("hit start");
				myScenario.setRunabble(true);
				myScenario.start();
				//Runnable runnable = new RunSim();
				//Thread thread = new Thread(runnable);
				//thread.start();
			}
		});
		controlPanel.add(startBtn, BorderLayout.WEST);
		frame.add(controlPanel,BorderLayout.NORTH);
		
		JTabbedPane tabbedPane = new JTabbedPane();
		
		TraceView traceView = new TraceView();
		Tracer.addListener(traceView);
		tabbedPane.add(traceView,"Trace");
		
		tabbedPane.add(mapView,"Map");
		
		tabbedPane.add(new JPanel(),"Data");

		frame.add(tabbedPane,BorderLayout.CENTER);
		
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
//			testEvent.test(testName);
		}
	}
}
