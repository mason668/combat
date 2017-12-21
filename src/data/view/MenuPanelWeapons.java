package data.view;

import java.awt.event.ActionListener;
import javax.swing.JLabel;
import javax.swing.JPanel;

public class MenuPanelWeapons extends MenuPanel {

	private static final long serialVersionUID = 1L;

	public MenuPanelWeapons(ActionListener actionListener) {
		super(actionListener);
		setLines(16);
		this.add(new JLabel ("Weapons"));
		this.add(makeButton("Weapon Characteristics", actionListener));
		this.add(makeButton("Additional Weapon Characteristics", actionListener));
		this.add(makeButton("Weapon PLOS Effects", actionListener));
		this.add(makeButton("Round Guidance", actionListener));
		this.add(makeButton("Suppressive Fire Effects", actionListener));
		
		{
			JPanel p1 = new JPanel();
			p1.add(makeButton("PH Data Sets", actionListener));
			p1.add(makeButton("PK Data Sets", actionListener));
			this.add(p1);
		}
		
		{
			JPanel p1 = new JPanel();
			p1.add(makeButton("Data Set by Weapon", actionListener));
			p1.add(makeButton("Data Set by Target", actionListener));
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(makeButton("Explosive Reactive Armour", actionListener));
			p1.add(makeButton("Active Defence Systems", actionListener));
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(makeButton("ERA Names", actionListener));
			p1.add(makeButton("ADS Names", actionListener));
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(makeButton("Soldier PK Data Sets", actionListener));
			p1.add(makeButton("Passenger PK Data Sets", actionListener));
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(makeButton("Crew PK Data Sets", actionListener));
			p1.add(makeButton("Rate of Fire Data", actionListener));
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(makeButton("MOPP Effects", actionListener));
			p1.add(makeButton("Area Fire Data", actionListener));
			this.add(p1);
		}

		this.add(new JLabel(" "));
		this.add(makeButton("Back", actionListener));
	}
}
