package view.janus_menu;

import java.awt.BorderLayout;
import java.awt.Color;

import javax.swing.JPanel;
import javax.swing.JTabbedPane;

public class InfoPanel extends JPanel{
	private static final long serialVersionUID = 1L;
	
	private SmallMap smallMap = new SmallMap();
	private ReportPanel reportPanel = new ReportPanel();
	
	public InfoPanel(){
		super();
		this.setBackground(Color.GREEN);
		this.setLayout(new BorderLayout());
		JTabbedPane tabbedPane = new JTabbedPane();
		tabbedPane.add("Map", smallMap);
		tabbedPane.add("Report", reportPanel);
		this.add(tabbedPane, BorderLayout.CENTER);
	}
	
}
