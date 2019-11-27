package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.JPanel;

public class Gridtest extends JPanel {

	public static void main(String[] args){
		Gridtest view = new Gridtest();
		
		view.setPreferredSize(new Dimension (500,500));
		
		FullFrame frame = new FullFrame("Test");

		frame.add(view,BorderLayout.CENTER);
		frame.setVisible(true);
		frame.pack();
		frame.validate();
	}
	
	public Gridtest(){
		this.setBackground(Color.blue);
		this.setLayout(new GridBagLayout());
		
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.weighty = 1.0;
		c.weightx = 0.5;
		c.gridy = 0;
		
		c.gridx = 0;
		c.gridy=0;
		JPanel p1 = new JPanel();
		p1.setBackground(Color.green);
		this.add(p1, c);
		
		c.gridx = 1;
		c.gridwidth=2;
		c.weightx = 1.0;
		c.gridy=0;
		JPanel p2 = new JPanel();
		p2.setBackground(Color.red);
		this.add(p2, c);

		c.gridx = 4;
		c.gridwidth=1;
		c.weightx = 0.5;
		c.gridy=0;
		JPanel p3 = new JPanel();
		p3.setBackground(Color.white);
		this.add(p3, c);
	}

}
