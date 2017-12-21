package view.janus_menu;

import java.awt.Color;

import javax.swing.JButton;
import javax.swing.JPanel;

public class ModeMenu extends JPanel{
	private static final long serialVersionUID = 1L;
	
	public ModeMenu(){
		super();
		this.setBackground(Color.CYAN);
		this.add(new JButton("Deploy"));
		this.add(new JButton("Start"));
		this.add(new JButton("Admin"));
	}

}
