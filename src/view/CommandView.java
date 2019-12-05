package view;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;

import data.CSData;
import interpreter.CommandInterpreter;
import sim.GameClock;
import sim.Scenario;
import sim.Simulation;
import sim.SimulationController;
import utils.Logger;
import utils.Tracer;
import view.reports.TraceView;

public class CommandView extends JPanel implements SimulationController{
	private static final long serialVersionUID = 1L;
	private Simulation mySimulation; 
	private SmallButton sendBtn = new SmallButton("Send");
	JTextField textFld = new JTextField("                                "); 

	public static void main(String[] args){
		FullFrame frame = new FullFrame("CommandView");
		CommandView view = new CommandView();
		frame.add(view,BorderLayout.NORTH);
		frame.setVisible(true);
		frame.pack();
		frame.validate();
	}
	
	public CommandView(){
		textFld = new JTextField(""); 
		textFld.setColumns(60);
		this.setLayout(new BorderLayout());
		textFld.setMinimumSize(new Dimension(50,10));
		this.add(sendBtn, BorderLayout.EAST);
		this.add(textFld, BorderLayout.CENTER);
		return;
	}
	
	public void setSimulation(Simulation sim){
		if ( sim == null) return;
		mySimulation = sim;
		sendBtn.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				mySimulation.interpret(textFld.getText());
				textFld.setText("                                  ");
			}
		});
	}
}
