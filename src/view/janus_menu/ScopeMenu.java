package view.janus_menu;

import java.awt.Color;

import javax.swing.JButton;
import javax.swing.JPanel;

public class ScopeMenu extends JPanel {
	private static final long serialVersionUID = 1L;
	
	public ScopeMenu(){
		super();
		this.setBackground(Color.GREEN);
		this.add(new JButton("Icon"));
		this.add(new JButton("Group"));
		this.add(new JButton("Formation"));
		this.add(new JButton("Passengers"));
		this.add(new JButton("Force"));
		this.add(new JButton("Side"));
	}

}
