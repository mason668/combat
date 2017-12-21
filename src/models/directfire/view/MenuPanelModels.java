package models.directfire.view;

import java.awt.event.ActionListener;
import javax.swing.JLabel;

import data.view.MenuPanel;

public class MenuPanelModels extends MenuPanel{

	private static final long serialVersionUID = 1L;

	public MenuPanelModels(ActionListener actionListener) {
		super(actionListener);
		setLines(4);
		this.add(new JLabel ("Models"));
		this.add(makeButton("Direct Fire Model", actionListener));
		this.add(new JLabel(" "));
		this.add(makeButton("Back", actionListener));
	}

}
