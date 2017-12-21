package data.view;

import java.awt.GridLayout;
import java.awt.event.ActionListener;

import javax.swing.JLabel;

public class MenuPanelChemical extends MenuPanel {

	private static final long serialVersionUID = 1L;

	public MenuPanelChemical(ActionListener actionListener) {
		super(actionListener);
		this.setLayout(new GridLayout(8,1,5,5));
		this.add(new JLabel ("Chemical & Heat Data"));
		this.add(makeButton("Chemical Susceptibility Data", actionListener));
		this.add(makeButton("Chemical Round Data", actionListener));
		this.add(makeButton("Heat Stress Data", actionListener));
		this.add(new JLabel(" "));
		this.add(new JLabel(" "));
		this.add(new JLabel(" "));
		this.add(makeButton("Back", actionListener));
	}
}
