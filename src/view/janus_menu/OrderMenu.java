package view.janus_menu;

import java.awt.Color;
import java.awt.Dimension;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

public class OrderMenu extends JPanel{
	private static final long serialVersionUID = 1L;
	
	public OrderMenu(){
		super();
		this.setBackground(Color.ORANGE);
//		this.setPreferredSize(new Dimension(100,500));
		JPanel myPanel = new JPanel();
//		JScrollPane scrollPane = new JScrollPane(myPanel);
//		scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		this.add(myPanel);
//		this.add(scrollPane);
		myPanel.setBackground(Color.CYAN);
		myPanel.setLayout(new BoxLayout(myPanel, BoxLayout.PAGE_AXIS));
		JPanel line1 = new JPanel();
		JPanel line2 = new JPanel();
		JPanel line3 = new JPanel();
		JPanel line4 = new JPanel();
		JPanel line5 = new JPanel();
		JPanel line6 = new JPanel();
		line1.setBackground(Color.CYAN);
		line2.setBackground(Color.CYAN);
		line3.setBackground(Color.CYAN);
		line4.setBackground(Color.CYAN);
		line5.setBackground(Color.CYAN);
		line6.setBackground(Color.CYAN);
		line1.add(new JButton("Route"));
		line1.add(new JButton("Alter"));
		line1.add(new JButton("Cancel"));
		line1.add(new JButton("Delete"));
		line1.add(new JButton("Copy"));
		line2.add(new JButton("Stop/Go"));
		line2.add(new JButton("Halt"));
		line2.add(new JButton("Go"));
		line3.add(new JButton("Move"));
		line3.add(new JButton("Show"));
		line3.add(new JButton("Speed"));
		line3.add(new JButton("Maximum"));
		line4.add(new JButton("Mount"));
		line4.add(new JButton("Transfer"));
		line4.add(new JButton("Assign"));
		line5.add(new JButton("Dismount"));
		line5.add(new JButton("Line"));
		line5.add(new JButton("Defilade"));
		line5.add(new JButton("Pit"));
		line6.add(new JButton("View"));
		line6.add(new JButton("Forward"));
		line6.add(new JButton("LOS"));
		line6.add(new JButton("Face"));

		myPanel.add(line1);
		myPanel.add(line2);
		myPanel.add(line3);
		myPanel.add(line4);
		myPanel.add(line5);
		myPanel.add(line6);
	}

}
