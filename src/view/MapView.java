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
import interpreter.MapInterpreter;
import utils.Logger;

public class MapView extends JPanel {
	private static final long serialVersionUID = 1L;
	private JPanel mapArea = new JPanel();
//	private JScrollPane mapPane = new JScrollPane(mapArea);
	private JPanel zoomArea = new JPanel();
	private JPanel menuArea = new JPanel();
	private Map myMap;

	public static void main(String[] args){
		Map map = new Map();
		MapInterpreter interpreter = new MapInterpreter();
		interpreter.setMap(map);
		interpreter.interpret(args);
		MapView view = new MapView(map);
		FullFrame frame = new FullFrame("MapView");

		//JPanel p = new JPanel();
		//p.add(view,BorderLayout.CENTER);
		//frame.add(p,BorderLayout.CENTER);
		
		frame.add(view,BorderLayout.CENTER);
//		frame.add(view.makeTestControler(),BorderLayout.NORTH);
		
		frame.setVisible(true);
		frame.pack();
		frame.validate();

	}
	
	public MapView(){
		super();
		init();
	}
	
	public MapView(Map map){
		super();
		myMap = map;
		init();
	}
	
	private void init(){
		this.setLayout(new BorderLayout());
		JScrollPane mapPane = new JScrollPane(mapArea);
		mapArea.setBackground(Color.WHITE);
		mapPane.setPreferredSize(new Dimension(1000,800));
		mapPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		mapPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
		this.add(mapPane,BorderLayout.CENTER);
		zoomArea.setBackground(Color.RED);
		this.add(zoomArea, BorderLayout.SOUTH);
		menuArea.setBackground(Color.BLACK);
		this.add(menuArea, BorderLayout.EAST);
	}
	
	public void setMap(Map map){
		myMap = map;
		Logger.log("set map " + map.getName());
		drawMap();
	}
	
	private void makeTestMap(Map map){
		
	}
	
	private int screenx;
	private int screeny;
	private void drawMap(){
		screenx = mapArea.getWidth();
		screeny = mapArea.getHeight();
	}
	
}
