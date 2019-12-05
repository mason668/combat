package view;

import java.awt.BorderLayout;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import sim.Simulation;
import utils.Parser;

/**
 * This class creates a JPanel containing a digital clock readout and
 * related clock control buttons.
 *
 */
public class ClockView extends JPanel implements ClockListener{
	//TODO the clock control buttons should be a separate component as they won't always go with the display
	private JTextField clockDisplay;
	private JButton pauseBtn;
	private JButton fastBtn;
	private JButton slowBtn;
	private JButton oneBtn;
	private JButton synchBtn;

	/**
	 * This class is executable. executing it creates a frame containing a ClockView.
	 * <p>
	 * The frame also contains basic simulation control functions and report panels for the Logger and Tracer.
	 * <p>
	 * It also creates an instance of a Simulation and links the clock and controls to it.
	 * @param args
	 */
	public static void main(String[] args){
		// make a frame for the GUI
		FullFrame frame = new FullFrame("ClockView");

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
	}
	
	public ClockView(){
		this.setLayout(new BorderLayout());
		makeComponents();
		JPanel displayPanel = new JPanel();
		displayPanel.add(new JLabel("Time: "));
		displayPanel.add(clockDisplay);
		this.add(displayPanel, BorderLayout.NORTH);
		JPanel controlPanel = new JPanel();
		controlPanel.add(pauseBtn);
		controlPanel.add(fastBtn);
		controlPanel.add(slowBtn);
		controlPanel.add(oneBtn);
		controlPanel.add(synchBtn);
		this.add(controlPanel, BorderLayout.CENTER);
	}
	
	private void makeComponents(){
		clockDisplay = new JTextField("00:00:00:00");
		pauseBtn = new SmallButton("Pause");
		fastBtn = new SmallButton("Fast");
		slowBtn = new SmallButton("Slow");
		oneBtn = new SmallButton("1:1");
		synchBtn = new SmallButton("No Synch");
	}

	@Override
	public void updateClock(double clock) {
		clockDisplay.setText(Parser.formatTime(clock));
	}
	
	public void addActionListener(ActionListener listener){
		pauseBtn.addActionListener(listener);
		fastBtn.addActionListener(listener);
		slowBtn.addActionListener(listener);
		oneBtn.addActionListener(listener);
		synchBtn.addActionListener(listener);
	}

}
