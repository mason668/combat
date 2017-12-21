package view.janus_menu;

import java.awt.Color;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;

public class MapMenu extends JPanel{
	private static final long serialVersionUID = 1L;
	
	public MapMenu(){
		super();
		this.setBackground(Color.BLUE);
		this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
		JPanel scaleLine = new JPanel();
		JPanel zoomLine = new JPanel();
		JPanel panLine = new JPanel();
		JPanel clutterLine = new JPanel();
		this.add(scaleLine);
		this.add(zoomLine);
		this.add(panLine);
		this.add(clutterLine);
		zoomLine.add(new JButton("Zoom"));
		zoomLine.add(new JButton("DF"));
		zoomLine.add(new JButton("1"));
		zoomLine.add(new JButton("2"));
		zoomLine.add(new JButton("3"));
		zoomLine.add(new JButton("4"));
		zoomLine.add(new JButton("5"));
		panLine.add(new JButton("Prev"));
		panLine.add(new JButton("Next"));
		panLine.add(new JButton("Pan"));
		panLine.add(new JButton("Grid"));
		clutterLine.add(new JButton("Clouds"));
		clutterLine.add(new JButton("Friends"));
		clutterLine.add(new JButton("Enemy"));
		clutterLine.add(new JButton("Hulks"));
		clutterLine.add(new JButton("Obstacles"));
	}

}
