package view.janus_menu;

import java.awt.BorderLayout;
import java.awt.Color;

import javax.swing.JPanel;
import javax.swing.JTabbedPane;

public class SpecialistMenu extends JPanel{
	private static final long serialVersionUID = 1L;
	
	public SpecialistMenu(){
		super();
		this.setBackground(Color.GREEN);
		this.setLayout(new BorderLayout());
		JTabbedPane tabbedPane = new JTabbedPane();
		this.add(tabbedPane, BorderLayout.CENTER);
		tabbedPane.add("Miscelaneous", new JPanel());
		tabbedPane.add("ISR", new JPanel());
		tabbedPane.add("Direct Fire", new JPanel());
		tabbedPane.add("Formations", new JPanel());
		tabbedPane.add("SOP", new JPanel());
		tabbedPane.add("Urban", new JPanel());
		tabbedPane.add("Indirect Fire", new JPanel());
		tabbedPane.add("Aviation", new JPanel());
		tabbedPane.add("Engineering", new JPanel());
		tabbedPane.add("Logistics", new JPanel());
	}

}
