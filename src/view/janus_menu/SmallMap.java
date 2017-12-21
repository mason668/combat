package view.janus_menu;

import java.awt.BorderLayout;
import java.awt.Color;

import javax.swing.JPanel;

public class SmallMap extends JPanel{
	private static final long serialVersionUID = 1L;
	private JPanel map = new JPanel();
	
	public SmallMap(){
		this.setLayout(new BorderLayout());
		map.setBackground(Color.BLUE);
		this.add(map, BorderLayout.CENTER);
	}
}
