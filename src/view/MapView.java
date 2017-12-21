package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Toolkit;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

import data.map.Map;

public class MapView extends JPanel {
	private static final long serialVersionUID = 1L;
	JPanel mapArea = new JPanel();
	Map myMap;

	public static void main(String[] args){
		Map map = new Map();
		MapView view = new MapView(map);
		FullFrame frame = new FullFrame("MapView");
		
		frame.add(view,BorderLayout.CENTER);
//		frame.add(view.makeTestControler(),BorderLayout.NORTH);
		
		frame.setVisible(true);
		frame.pack();
		frame.validate();

	}
	
	public MapView(Map map){
		super();
		myMap = map;
		this.setLayout(new BorderLayout());
		JScrollPane pane = new JScrollPane(mapArea);
		mapArea.setBackground(Color.WHITE);
		pane.setPreferredSize(new Dimension(100,100));
		pane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		pane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
		this.add(pane,BorderLayout.CENTER);
	}
	
}
