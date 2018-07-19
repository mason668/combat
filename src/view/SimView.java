package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;

import javax.swing.BoxLayout;
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
import view.menu.MenuController;

public class SimView implements ChangeListener{
	
	private static String label = "Simulation";
	private Simulation mySimulation;
	
	private StatusBar statusBar;
	private MapView mapView = new MapView();
	private CommandView commandView = new CommandView();
	private ClockView clockView = new ClockView();
	private MapPanel smallMap = new MapPanel();
	private MenuPanel menuPanel = new MenuPanel();
	private JButton startBtn = new SmallButton("Start");
	private TraceView traceView;
	private ReportView reportView;
	private JTabbedPane tabbedPane;
	private SpriteManager mySpriteManager;
	private MenuController menuController;
	private final int REPORT_PANE = 0;
	private final int LOG_PANE = 1;
	private final int TRACE_PANE = 2;
	
	//private SpriteManager spriteManager = new SpriteManager();

	public static void main(String[] args){
		Logger.setEcho(true);
		Tracer.setEcho(true); //TODO temp so we get traces
		Tracer.setTraceLevel(Tracer.FINEST); //TODO we need this in the interpreter
		SimView test = new SimView(label, args);
	}

	public SimView(String label, String[] args) {
		makeGUI(label);
		mySimulation = new Simulation();
		mySpriteManager = new SpriteManager();
		linkGUItoSim();
		//initialise(args);
		initialise(new String[]{"--load", "init.txt"});
	}
	
	private void initialise(String[] args){
		mySimulation.getInterpreter().setTrace(true); // TODO just for testing
		mySimulation.init(args);
		mapView.setMap(mySimulation.getScenario().getMap());
		mapView.makeVisible(true);
		smallMap.setMap(mySimulation.getScenario().getMap());
		smallMap.makeVisible(true);
	}

	protected void makeGUI(String label){
		FullFrame frame = new FullFrame(label);
		JPanel panel = new JPanel();
		panel.setBackground(Color.cyan);
		makeComponents();
		positionComponents(panel);

		frame.add(panel,BorderLayout.CENTER);
		frame.setVisible(true);
		frame.pack();
		frame.validate();
	}
	
	private void linkGUItoSim(){
		mySimulation.addControl(commandView);
		mySimulation.addClockListener(clockView);
		clockView.addActionListener(mySimulation.getGameClock().getClockController());
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
		commandView.setInterpreter(mySimulation.getInterpreter());
		smallMap.setSpriteManager(mySpriteManager);
		mapView.setSpriteManager(mySpriteManager);
		mapView.addMapListener(menuController);
		mySpriteManager.setEntityList(mySimulation.getScenario().getEntityList());
		mySpriteManager.setmenuController(menuController);
		Tracer.addListener(traceView);
		reportView.setMenuController(menuController);
		menuPanel.setMenuController(menuController);
		menuController.setScenario(mySimulation.getScenario());
		// TODO add report listener
		menuController.addReportLitener(reportView);
	}
	
	private void makeComponents(){
		statusBar = new StatusBar();
		mapView = new MapView();
		commandView = new CommandView();
		clockView = new ClockView();
		smallMap = new MapPanel();
		startBtn = new SmallButton("Start");
		traceView = new TraceView();
		reportView = new ReportView();
		menuController = new MenuController();
	}
	
	private void positionComponents(JPanel panel){
		panel.setLayout(new BorderLayout());
		panel.add(makeMainPanel(),BorderLayout.CENTER);
		panel.add(statusBar,BorderLayout.SOUTH);
	}
	
	private JPanel makeMainPanel(){
		JPanel panel = new JPanel();
		
		panel.setBackground(Color.blue);
		panel.setLayout(new GridBagLayout());
		
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.weighty = 1.0;
		c.gridy = 0;

		MapPanel mp = new MapPanel();
		//JPanel mp = new JPanel();
		mp.setBackground(Color.GREEN);
		
		c.gridx = 0;
		c.weightx = 1.0;
		//c.gridwidth = 3; // does this help?
		panel.add(mapView, c);

		c.gridx = 3;
		//c.gridwidth = 1;
		c.weightx = 0.3;
		panel.add(makeSidePanel(), c);
		
		return panel;
	}
	
	private JPanel makeSidePanel(){
		return makeSidePanelGridbag();
	}
	
	private JPanel makeSidePanelBox(){
		JPanel panel = new JPanel();
		panel.setBackground(Color.cyan);
		panel.setLayout(new BoxLayout(panel,BoxLayout.PAGE_AXIS));
		panel.add(makeControlPanel());
		panel.add(menuPanel);
		panel.add(makeReportPanel());
		panel.add(smallMap);
		return panel;
	}
	
	private JPanel makeSidePanelGridbag(){
		JPanel panel = new JPanel();
		panel.setBackground(Color.cyan);

		panel.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.gridx = 0;
		c.gridwidth = 1;
		c.weightx = 0.5;
		c.gridheight = 1;

		c.weighty = 0.5;
		c.gridy = 0;
		panel.add(makeControlPanel(), c);

		c.weighty = 0.5;
		c.gridy = 1;
		panel.add(menuPanel, c);

		c.weighty = 0.5;
		c.gridy = 2;
		panel.add(makeReportPanel(), c);

		c.weighty = 1.0;
		c.gridy = 3;
		c.gridheight = 2;
		panel.add(smallMap, c);
		
		return panel;
	}

	private JPanel makeReportPanel(){
		JPanel panel = new JPanel();
		panel.setBackground(Color.pink);
		panel.setLayout(new BorderLayout());

		tabbedPane = new JTabbedPane();
		//tabbedPane.addChangeListener(this);
		
		tabbedPane.insertTab("Report", null, reportView, "Display reports", REPORT_PANE);
		tabbedPane.insertTab("Log", null, new JPanel(), "Display log messages", LOG_PANE);
		tabbedPane.insertTab("Trace", null, traceView, "Display trace messages", TRACE_PANE);
		
		//panel.add(traceView, BorderLayout.CENTER);
		panel.add(tabbedPane);
		panel.addComponentListener(new ComponentListener(){

			@Override
			public void componentHidden(ComponentEvent arg0) {}
			@Override
			public void componentMoved(ComponentEvent arg0) {}
			@Override
			public void componentShown(ComponentEvent arg0) {}

			@Override
			public void componentResized(ComponentEvent arg0) {
				/*
				int width = panel.getWidth()-20;
				int height = panel.getHeight()-20;
				width = Math.min(width, 500);
				height = Math.min(height, 300);
				Logger.say("SimView resized " + panel.getWidth() + ":" + panel.getHeight() +
						"   " + width + ":" + height);
				tabbedPane.setPreferredSize(new Dimension( width, height));
				//traceView.setPreferredSize(new Dimension( width, height));
				
				 */
			}

		});

		return panel;
	}

	private JPanel makeMenuPanela(){
		JPanel panel = new JPanel();
		panel.setBackground(Color.orange);
		return panel;
	}
	
	private JPanel makeControlPanel(){
		JPanel panel = new JPanel();
		panel.setBackground(Color.yellow);
		panel.setLayout(new BoxLayout(panel, BoxLayout.PAGE_AXIS));
		//panel.add(commandView,BorderLayout.CENTER);
		//panel.add(clockView, BorderLayout.SOUTH);
		//panel.add(startBtn, BorderLayout.NORTH);
		panel.add(startBtn);
		panel.add(commandView);
		panel.add(clockView);
		return panel;
	}
	
	@Override
	public void stateChanged(ChangeEvent arg0) {
		Logger.say("help me");
		/*
		if (tabbedPane.getSelectedIndex() == MAP_PANE){
			mapView.makeVisible(true);
		} else {
			mapView.makeVisible(false);
		}
		*/
		
	}
}
