package view;

import java.awt.BorderLayout;

import javax.swing.JPanel;
import javax.swing.JTabbedPane;

import utils.Logger;
import utils.Tracer;
import view.reports.LogView;
import view.reports.TraceView;

public class TabView extends JPanel {
	
	private JTabbedPane tabbedPane;
	private TraceView traceView;
	private LogView logView;
	private int tabs = 2;

	public TabView (){
		this.setLayout(new BorderLayout());
		tabbedPane = new JTabbedPane();
		traceView = new TraceView();
		logView = new LogView();
		tabbedPane.insertTab("Log", null, logView, "Display log messages", 0);
		tabbedPane.insertTab("Trace", null, traceView, "Display trace messages", 1);
		this.add(tabbedPane,BorderLayout.CENTER);
		Tracer.addListener(traceView);
		Logger.addListener(logView);
	}
	
	public void addTab(String label, JPanel panel){
		tabbedPane.insertTab(label, null, panel, "", tabs);
		tabbedPane.setSelectedIndex(tabs);
		tabs++;
	}
	
	public void addTab(String label, JPanel panel, int index){
		tabbedPane.insertTab(label, null, panel, "", index);
		tabbedPane.setSelectedIndex(index);
		tabs++;
	}

}
