package data.view;


import java.awt.GridLayout;
import java.awt.event.ActionListener;


import javax.swing.JLabel;

public class MenuPanelMain extends MenuPanel{

	private static final long serialVersionUID = 1L;

	public MenuPanelMain(ActionListener actionListener) {
		super(actionListener);
		setLines(12);
		this.add(new JLabel ("Main Menu"));
		this.add(makeButton("Settings", actionListener));
		this.add(makeButton("Platforms", actionListener));
		this.add(makeButton("Weapons", actionListener));
		this.add(makeButton("Sensors", actionListener));
		this.add(makeButton("Engineer Data", actionListener));
		this.add(makeButton("Weather", actionListener));
		this.add(makeButton("Chemical & Heat Data", actionListener));
		this.add(new JLabel(" "));
		this.add(makeButton("Models", actionListener));
		this.add(new JLabel(" "));
		this.add(makeButton("Exit", actionListener));
	}

}
