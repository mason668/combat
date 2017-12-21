package data.view.platforms;

import java.awt.event.ActionListener;
import javax.swing.JLabel;
import javax.swing.JPanel;

import data.view.MenuPanel;

public class MenuPanelPlatformCharacteristics extends MenuPanel {

	private static final long serialVersionUID = 1L;

	public MenuPanelPlatformCharacteristics(ActionListener actionListener) {
		super(actionListener);
		setLines(15);
		this.add(new JLabel ("Platform Characteristics"));
		
		this.add(makeButton("Platform Characteristics 1", actionListener));
		this.add(makeButton("Platform Characteristics 2", actionListener));
		this.add(makeButton("Platform Characteristics 3", actionListener));
		{
			JPanel p1 = new JPanel();
			p1.add(makeButton("Detection Heights", actionListener));
			p1.add(makeButton("Posture Height", actionListener));
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(makeButton("Symbology Data", actionListener));
			p1.add(makeButton("Speed Data", actionListener));
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(makeButton("Detection Data", actionListener));
			p1.add(makeButton("Optical and Thermal Contrast", actionListener));
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(makeButton("Mine Clearing Data", actionListener));
			p1.add(makeButton("Mine Detection Data", actionListener));
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(makeButton("Mine Dispenser Data", actionListener));
			p1.add(makeButton("Active Defence Systems", actionListener));
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(makeButton("Volume and Weight", actionListener));
			p1.add(makeButton("POL Usage", actionListener));
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(makeButton("Suppression Data", actionListener));
			p1.add(makeButton("Sensor Data", actionListener));
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(makeButton("Obstacle Crossing Data", actionListener));
			p1.add(makeButton("Field of View Data", actionListener));
			this.add(p1);
		}
		this.add(makeButton("Day & Night Data", actionListener));

		this.add(makeButton("Back", actionListener));
	}
}
