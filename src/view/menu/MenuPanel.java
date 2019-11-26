package view;

import java.awt.Color;

import javax.swing.BoxLayout;
import javax.swing.JPanel;

import view.menu.DeployButton;
import view.menu.MenuButton;
import view.menu.MenuController;
import view.menu.MoveButton;

public class MenuPanel extends JPanel{
	
	DeployButton deployButton = new DeployButton();
	MoveButton moveButton = new MoveButton();

	public MenuPanel(){
		super();
		setBackground(Color.WHITE);
		addButtons();
	}
	
	private void addButtons(){
		//TODO change to menubuttons
		
		JPanel line1 = new JPanel();
		line1.setBackground(Color.BLUE);
		JPanel line2 = new JPanel();
		line2.setBackground(Color.RED);
		JPanel line3 = new JPanel();
		line3.setBackground(Color.GREEN);
		
		line1.add(deployButton);
		line1.add(moveButton);
		
		this.setLayout(new BoxLayout(this,BoxLayout.PAGE_AXIS));
		this.add(line1);
		this.add(line2);
		this.add(line3);
	}

	public void setMenuController(MenuController controller){
		if (controller == null) return;
		controller.addMenuButton(deployButton);
		controller.addMenuButton(moveButton);
	}
}
