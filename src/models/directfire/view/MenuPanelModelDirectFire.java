package models.directfire.view;

import java.awt.event.ActionListener;
import javax.swing.JLabel;

import data.view.MenuPanel;

public class MenuPanelModelDirectFire extends MenuPanel{

	private static final long serialVersionUID = 1L;

	public MenuPanelModelDirectFire(ActionListener actionListener) {
		super(actionListener);
		setLines(5);
		this.add(new JLabel ("Direct Fire Model"));
		this.add(makeButton("Test PH", actionListener));
		this.add(makeButton("Test PH", actionListener));
		this.add(new JLabel(" "));
		this.add(makeButton("Back", actionListener));
	}

}
