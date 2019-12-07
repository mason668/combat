package view.models;

import java.awt.BorderLayout;
import java.awt.Color;
import java.util.HashMap;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;

import sim.Simulation;
import sim.entity.MoverEntity;
import sim.listeners.MovementListener;
import utils.Parser;
import view.ClockView;
import view.FullFrame;
import view.StartView;
import view.TabView;

public class MovementOutputPanel extends JPanel implements MovementListener {

	private JTable table;
	private Simulation mySimulation;
	private JScrollPane scrollPane;
	private DefaultTableModel model = new DefaultTableModel();

	public static void main(String[] args){
		// make a frame for the GUI
		FullFrame frame = new FullFrame("Movement");

		// make a clock view and add to the frame
		ClockView clockView = new ClockView();
		frame.add(clockView,BorderLayout.NORTH);
		
		// Make a control and report panel and add to the frame
		JPanel reportPanel = new JPanel();
		reportPanel.setLayout(new BorderLayout());

		TabView tab = new TabView();
		MovementOutputPanel me = new MovementOutputPanel();
		tab.addTab("Move", me);
		StartView startPanel = new StartView();
		
		reportPanel.add(tab, BorderLayout.CENTER);
		reportPanel.add(startPanel, BorderLayout.NORTH);

		frame.add(reportPanel,BorderLayout.CENTER);
		
		// make the frame visible etc
		frame.setVisible(true);
		frame.pack();
		frame.validate();

		// instantiate a simulation and link the GUI components
		Simulation mySim = new Simulation(false);
		mySim.initialise(args);
		me.setSimulation(mySim);

		mySim.addClockListener(clockView);
		clockView.addActionListener(mySim.getGameClock().getClockController());
		startPanel.setSimulation(mySim);
	}

	public MovementOutputPanel(){
		this.setLayout(new BorderLayout());
		this.setBackground(Color.GREEN);
		
		model.addColumn("Entity");
		model.addColumn("Location");
		model.addColumn("Speed");
		model.addColumn("Fuel");
		model.addColumn("Direction");
		model.addColumn("Facing");

		table = new JTable(model);
		table.setFillsViewportHeight(true);
		scrollPane = new JScrollPane(table, 
				JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, 
				JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		this.add(scrollPane, BorderLayout.CENTER);
	}
	
	public void setSimulation( Simulation sim){
		if (sim == null) return;
		mySimulation = sim;
		mySimulation.addMovementListener(this);
	}

	class MovementRecord{
		public MoverEntity entity;
		public double fuel;
	}
	
	private HashMap<MoverEntity,Integer> movementTable = 
			new HashMap<MoverEntity,Integer>();

	@Override
	public void update(MoverEntity entity) {
		Integer index = movementTable.get(entity);
		if (index == null){
			Integer i = table.getRowCount();
			movementTable.put(entity, i);
			model.addRow(
				new String[]{
					entity.getID(),
					entity.getLocation().toGrid(),
					""+entity.getCurrentSpeed(),
					""+entity.getCurrentFuel(),
					""+Parser.bearing(entity.getDirectionMove()),
					""+Parser.bearing(entity.getDirectionFace())
				} );
		} else {
			int row = index;
			model.setValueAt(entity.getID(), row, 0);
			model.setValueAt(entity.getLocation().toGrid(), row, 1);
			model.setValueAt(""+entity.getCurrentSpeed(), row, 2);
			model.setValueAt(""+entity.getCurrentFuel(), row, 3);
			model.setValueAt(""+Parser.bearing(entity.getDirectionMove()), row, 4);
			model.setValueAt(""+Parser.bearing(entity.getDirectionFace()), row, 5);
		}
	}
	
}
