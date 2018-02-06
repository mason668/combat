package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Toolkit;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

import data.map.Map;
import interpreter.MapInterpreter;
import utils.Logger;

public class MapView extends JPanel{
	private static final long serialVersionUID = 1L;
	private MapPanel mapArea = new MapPanel();
//	private JScrollPane mapPane = new JScrollPane(mapArea);
	private JPanel zoomArea = new JPanel();
	private JPanel menuArea = new JPanel();
	private MapControl mapControl = new MapControl();
	private MapInfo mapInfo;
	private Map myMap;
	
	public static void main(String[] args){
		Map map = new Map();
		map.makeTestMap();
		MapInterpreter interpreter = new MapInterpreter();
		interpreter.setMap(map);
		interpreter.interpret(args);
		map.trace();
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
		view.makeVisible(true);
	}
	
	public MapView(Map map){
		super();
		this.setMap(map);
		init();
	}
	
	private void init(){
		mapInfo = new MapInfo(myMap);
		this.setLayout(new BorderLayout());
//		JScrollPane mapPane = new JScrollPane(mapArea);
		mapArea.setBackground(Color.WHITE);
//		mapPane.setPreferredSize(new Dimension(1000,800));
//		mapPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
//		mapPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
		this.add(mapArea,BorderLayout.CENTER);
//		this.add(mapPane,BorderLayout.CENTER); //TODO add scrollpane back again
		zoomArea.setBackground(Color.WHITE);
		zoomArea.add(mapInfo);
		zoomArea.add(mapControl);
		JScrollPane controlPane = new JScrollPane(zoomArea);
		controlPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
		controlPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
//		this.add(zoomArea, BorderLayout.SOUTH);
		this.add(controlPane, BorderLayout.SOUTH);
		menuArea.setBackground(Color.BLACK);
		this.add(menuArea, BorderLayout.EAST);
		mapArea.addMapListener(mapInfo);
	}
	
	public void setMap(Map map){
		myMap = map;
		Logger.log("MapView: set map " + map.getName());
		mapArea.setMap(myMap);
		// TODO update mapinfo too?
		//drawMap();
	}
	
	public void makeVisible(boolean b){
		this.mapArea.makeVisible(b);
	}
	
}
