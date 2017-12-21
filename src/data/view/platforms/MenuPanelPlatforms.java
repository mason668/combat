package data.view.platforms;

import java.awt.event.ActionListener;

import javax.swing.JLabel;

import data.view.MenuPanel;

public class MenuPanelPlatforms extends MenuPanel{

	private static final long serialVersionUID = 1L;

	public MenuPanelPlatforms(ActionListener actionListener) {
		super(actionListener);
		setLines(16);
		this.add(new JLabel ("Platforms"));
		this.add(makeButton("Platform Characteristics", actionListener));
		this.add(new JLabel(" "));
		this.add(makeButton("Weapon Basic Load", actionListener));
		this.add(makeButton("Weapon Selection by Firer", actionListener));
		this.add(makeButton("Weapon Selection by Target", actionListener));
		this.add(new JLabel(" "));
		this.add(makeButton("Vulnerability to Indirect Fire", actionListener));
		this.add(makeButton("Repairs", actionListener));
		this.add(new JLabel(" "));
		this.add(makeButton("Artillery Systems", actionListener));
		this.add(makeButton("Aircraft Systems", actionListener));
		this.add(makeButton("RADAR Systems", actionListener));
		this.add(makeButton("Fuel Systems", actionListener));
		this.add(new JLabel(" "));
		this.add(makeButton("Back", actionListener));
	}

}
