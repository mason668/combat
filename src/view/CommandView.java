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
import sim.Scenario;
import utils.Tracer;

public class CommandView extends JPanel {
	private static final long serialVersionUID = 1L;
	private CommandInterpreter interpreter;

	public static void main(String[] args){
		CommandView view = new CommandView();
		CommandInterpreter i = new CommandInterpreter(new CSData(), new Scenario());
		i.setTrace(true);
		view.setInterpreter(i);
		FullFrame frame = new FullFrame("CommandView");
		frame.add(view,BorderLayout.NORTH);
		TraceView trace = new TraceView();
		Tracer.addListener(trace);
		frame.add(trace,BorderLayout.CENTER);
//		frame.add(view.makeControlPanel(),BorderLayout.SOUTH);
		frame.setVisible(true);
		frame.pack();
		frame.validate();
	}
	
	public CommandView(){
		JTextField text = new JTextField(100);
		JButton testBtn = new JButton("Send");
		testBtn.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				if (interpreter != null){
					interpreter.interpret(text.getText());
				}
				text.setText("");
			}
		});
		this.add(testBtn, BorderLayout.EAST);
		this.add(text, BorderLayout.CENTER);
		return;
	}
	
	public void setInterpreter(CommandInterpreter commandInterpreter){
		this.interpreter = commandInterpreter;
	}

}
