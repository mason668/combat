package view;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import data.CSData;
import interpreter.CommandInterpreter;
import sim.Scenario;
import sim.Simulation;
import utils.Logger;
import utils.Tracer;
import view.ClockView;
import view.CommandView;
import view.FullFrame;
import view.TraceView;

public class SimView implements ChangeListener{
	
	private static String label = "Simulation";
	private static String testName = "TestMoveEvent";
	private Scenario myScenario = new Scenario("sim gui", false);
	private CSData myData = new CSData();
	private CommandInterpreter myInterpreter = new CommandInterpreter(myData, myScenario);
	
	private MapView mapView;// = new MapView();
	private JTabbedPane tabbedPane;
	private final int TRACE_PANE = 0;
	private final int MAP_PANE = 1;
	private final int DATA_PANE = 2;

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
//		myScenario.getMap().makeTestMap();
//		myScenario.getMap().trace();
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
		
		tabbedPane = new JTabbedPane();
		tabbedPane.addChangeListener(this);
		
		TraceView traceView = new TraceView();
		Tracer.addListener(traceView);
		
		JPanel dataView = new JPanel();
		
		mapView = new MapView(myScenario.getMap());
		
		tabbedPane.insertTab("Log", null, traceView, "Display log data", TRACE_PANE);
		tabbedPane.insertTab("Map", null, mapView, "Display map", MAP_PANE);
		tabbedPane.insertTab("Data", null, dataView, "Display data", DATA_PANE);

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

	@Override
	public void stateChanged(ChangeEvent arg0) {
		if (tabbedPane.getSelectedIndex() == MAP_PANE){
			mapView.makeVisible(true);
		} else {
			mapView.makeVisible(false);
		}
		
	}
}
