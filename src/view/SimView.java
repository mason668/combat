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
import sim.GameClock;
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
	private Simulation mySimulation = new Simulation();
	
	private MapView mapView;// = new MapView();
	private JTabbedPane tabbedPane;
	private final int TRACE_PANE = 0;
	private final int MAP_PANE = 1;
	private final int DATA_PANE = 2;
	
	private SpriteManager spriteManager = new SpriteManager();

	public static void main(String[] args){
		SimView test = new SimView(label, args);
	}

	public SimView(String label, String[] args) {
		makeFrame(label);
		mySimulation.getInterpreter().setTrace(true);
		if (args.length<=0){
			mySimulation.getInterpreter().interpret("load tests/test_move.txt");
//			mySimulation.init(new String[] {"--load init.txt"});
		} else {
			mySimulation.init(args);
		}
		//mapView.setMap(mySimulation.getScenario().getMap()); // TODO done twice?
//		myScenario.getMap().makeTestMap();
//		myScenario.getMap().trace();
	}

	protected void makeFrame(String label){
		FullFrame frame = new FullFrame(label);

		JPanel controlPanel = new JPanel();
		controlPanel.setLayout(new BorderLayout());

		CommandView commandView = new CommandView();
		commandView.setInterpreter(mySimulation.getInterpreter());
		controlPanel.add(commandView,BorderLayout.CENTER);

		ClockView clockView = new ClockView();
		mySimulation.addClockListener(clockView);
		clockView.addActionListener(mySimulation.getGameClock().getClockController());
		controlPanel.add(clockView, BorderLayout.SOUTH);
		
		JButton startBtn = new JButton("Start");
		startBtn.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				Tracer.write("hit start");
				//myScenario.setRunabble(true);
				mySimulation.startSimulation();
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
		
		mapView = new MapView(mySimulation.getScenario().getMap()); //TODO this appears to be done twice
		
		tabbedPane.insertTab("Log", null, traceView, "Display log data", TRACE_PANE);
		tabbedPane.insertTab("Map", null, mapView, "Display map", MAP_PANE);
		tabbedPane.insertTab("Data", null, dataView, "Display data", DATA_PANE);

		frame.add(tabbedPane,BorderLayout.CENTER);
		
		//		frame.add(view.makeControlPanel(),BorderLayout.SOUTH);
		frame.setVisible(true);
		frame.pack();
		frame.validate();
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
