package view.janus_menu;

import java.awt.BorderLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.BoxLayout;
import javax.swing.JFrame;

public class Workstation extends JFrame {
	private static final long serialVersionUID = 1L;

	public static void main(String args[]){
		Workstation me = new Workstation();
		me.test();
	}
	
	private void test(){
	}
	
	private SidePanel sidePanel = new SidePanel();
	private BigMap bigMap = new BigMap();
	private StatusBar statusPanel = new StatusBar();
	
	public Workstation(){
		super("Workstation");
		this.addWindowListener(
				new WindowAdapter(){
					public void windowClosing(WindowEvent event){
						System.exit(0);;
					}
				}
		);
		this.setSize(800, 600);
		this.setLayout(new BorderLayout());
		this.add(bigMap, BorderLayout.CENTER);
		this.add(statusPanel, BorderLayout.SOUTH);
		this.add(sidePanel, BorderLayout.EAST);
		
		this.setVisible(true);
		this.validate();
	}

}
