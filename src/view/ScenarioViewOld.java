package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

import sim.Scenario;

public class ScenarioViewOld extends JPanel {
	private static final long serialVersionUID = 1L;
	
	public static void main(String[] args){
		JFrame frame = new JFrame("scenario");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setPreferredSize(new Dimension(800, 300));
		frame.setMinimumSize(new Dimension(100,100));
		
		ScenarioViewOld view = new ScenarioViewOld(new Scenario());
		
		frame.add(view,BorderLayout.CENTER);
		frame.setVisible(true);
		frame.pack();
		frame.validate();
		
		JPanel panel = new JPanel();
		JButton testBtn = new JButton("test");
		panel.add(testBtn);
		frame.add(panel,BorderLayout.SOUTH);
		testBtn.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				//Tracer.write("another test");
			}
		});
	}
	
	private Scenario myScenario;
	
	public ScenarioViewOld(Scenario scenario){
		super();
		myScenario = scenario;
		this.setLayout(new BorderLayout());
		JTabbedPane tabbedPane = new JTabbedPane();
		this.add(tabbedPane, BorderLayout.CENTER);
		tabbedPane.addTab("General", makePanel1());
		tabbedPane.addTab("Timing", new JPanel());
		tabbedPane.addTab("Options", new JPanel());
	}
	
	private JPanel makePanel1(){
		JPanel panel = new JPanel();
		int lines = 1;
		this.setLayout(new GridLayout(lines,1,5,5));
//		panel.setLayout(new GridLayout(0,4));
		JPanel line1 = new JPanel();
		line1.add(new JLabel("start time"));
//		line1.add(txtStartTime);
		line1.add(new DataField("start", "aaa", new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				System.out.println("txt changed");
			}
		}));
		line1.add(new JLabel("end time"));
		panel.add(line1);
		return panel;
	}
	
	//private JTextField txtStartTime = new JTextField("0.0");
	
	private class TxtStartTime extends JPanel{
		private static final long serialVersionUID = 1L;
		JTextField myField = new JTextField();
		JLabel myLabel = new JLabel("Start Time");
		private TxtStartTime (){
			this.setPreferredSize(new Dimension(200,25));
//			this.setLayout(new FlowLayout());
			this.setLayout(new GridLayout(0,2));
			myField.setText("0.0");
			myField.setPreferredSize(new Dimension(20,20));
			myField.setMinimumSize(new Dimension(40,20));
			myField.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent arg0) {
					System.out.println("txt changed");
				}
			});
			this.add(myLabel);
			this.add(myField);
			this.setBackground(Color.gray);
		}
	}

}
