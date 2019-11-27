package view.map;

import java.awt.Color;

import javax.swing.JPanel;
import javax.swing.JTextArea;

public class MapControl extends JPanel {
	
	private JTextArea temp = new JTextArea("This is where the zoom controls will go");
	
	public MapControl(){
		super();
		this.setBackground(Color.WHITE);
		this.add(temp);
	}

}
