package view;

import java.awt.Color;

import javax.swing.BoxLayout;
import javax.swing.JPanel;

import view.menu.MenuButton;

public class MenuPanel extends JPanel{
	
	public MenuPanel(){
		super();
		setBackground(Color.RED);
		addButtons();
	}
	
	private void addButtons(){
		//TODO change to menubuttons
		MenuButton deployButton = new MenuButton("Deploy");
		
		JPanel line1 = new JPanel();
		line1.setBackground(Color.BLUE);
		JPanel line2 = new JPanel();
		line2.setBackground(Color.RED);
		JPanel line3 = new JPanel();
		line3.setBackground(Color.GREEN);
		
		line1.add(deployButton);
		
		this.setLayout(new BoxLayout(this,BoxLayout.PAGE_AXIS));
		this.add(line1);
		this.add(line2);
		this.add(line3);
	}

}
