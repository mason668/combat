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
	//private MapControl mapControl = new MapControl(); TODO add zoom buttons
	private MapInfo mapInfo;
	private Map myMap;
	private boolean mapLoaded = false;
	
	public static void main(String[] args){
		Map map = new Map();
		map.makeTestMap();
		MapView view = new MapView();
		FullFrame frame = new FullFrame("MapView");

		frame.add(view,BorderLayout.CENTER);
		
		frame.setVisible(true);
		frame.pack();
		frame.validate();
		view.makeVisible(true);
		
		Logger.setEcho(true);
		MapInterpreter interpreter = new MapInterpreter();
		interpreter.setMap(map);
		interpreter.interpret(args);
		view.setMap(map);
	}
	
	public MapView(){
		super();
		init();
	}
	
	private void init(){
		mapInfo = new MapInfo();
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
		//zoomArea.add(mapControl); TODO add zom buttons
		JScrollPane controlPane = new JScrollPane(zoomArea);
		controlPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
		controlPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
//		this.add(zoomArea, BorderLayout.SOUTH);
		this.add(controlPane, BorderLayout.SOUTH);
		//menuArea.setBackground(Color.BLACK);
		//this.add(menuArea, BorderLayout.EAST);
		mapArea.addMapListener(mapInfo);
	}
	
	public void setMap(Map map){
		myMap = map;
		Logger.log("MapView: set map " + map.getName());
		mapArea.setMap(myMap);
		//mapArea.setVisible(true);
		mapInfo.setMap(myMap);
		//drawMap();
		mapLoaded = true;
	}
	
	public void makeVisible(boolean b){
		this.mapArea.makeVisible(b);
	}
	
	public void setSpriteManager(SpriteManager manager){
		mapArea.setSpriteManager(manager);
	}
	
}
