package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;

import utils.Parser;

public class SimpleView extends JFrame implements ActionListener, ClockListener, TraceListener{
	
	JPanel p1 = new JPanel();
	JTextArea t1 = new JTextArea();
	JTextArea t2 = new JTextArea();

	public SimpleView(){
		super("Simple View");
		registerEventHandlers();
		this.setSize(800, 600);
		this.setVisible(true);
		this.add(new StatusBar(), BorderLayout.SOUTH);
		this.add(new GamePanel(), BorderLayout.CENTER);
		this.validate();
	
	}

	@Override
	public void write(String message) {
		t1.append(message+"\n");
//		System.out.println("SV:" + message);
		// TODO Auto-generated method stub
		
	}

	@Override
	public void updateClock(double clock) {
		t2.setText(Parser.formatTime(clock));
		// TODO Auto-generated method stub
		
	}
	
	private void registerEventHandlers(){
		this.addWindowListener(
				new WindowAdapter(){
					public void windowClosing(WindowEvent event){
						System.exit(0);;
					}
				}
		);
	}

	@Override
	public void actionPerformed(ActionEvent arg0) {
		// TODO Auto-generated method stub
		
	}
	
class StatusBar extends JPanel {
	public StatusBar(){
		this.setBackground(Color.red);
	}
}

class GamePanel extends JPanel{
	public GamePanel(){
		this.setBackground(Color.green);
		JTabbedPane tabs = new JTabbedPane();
		this.add(tabs, BorderLayout.CENTER);
		tabs.addTab("map", new MapPanel());
		tabs.addTab("data", new TablePanel());
		
	}
}

class MapPanel extends JPanel{
	public MapPanel(){
		this.setBackground(Color.blue);
//		this.setPreferredSize(new Dimension(1000,1000));
	}
}

class TablePanel extends JPanel{
	public TablePanel(){
		this.setBackground(Color.CYAN);
	}
}

}
