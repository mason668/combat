package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Polygon;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.JPanel;

import data.map.Area;
import data.map.AreaFeature;
import data.map.Building;
import data.map.Coordinate;
import data.map.Feature;
import data.map.Map;
import data.map.MapTransposer;
import data.map.River;
import data.map.Road;
import interpreter.MapInterpreter;
import utils.Logger;

public class MapPanel extends JPanel implements Runnable, MouseListener, MouseMotionListener{

	private Thread animator;
	private volatile boolean running = false;
	private volatile boolean visible = false;
	private volatile boolean redraw = true;
	private Map myMap;
//	private long period = 1000; // miliseconds
	private long period = 1000;
	private JPanel mapArea = new JPanel();
	private MapTransposer mapTransposer = new MapTransposer();
	private Vector<MapListener> mapListeners = new Vector<MapListener>();
	private boolean relief = false;
	private SpriteManager mySpriteManager;

	public static void main(String[] args){
		
		Map testMap = new Map();
		if (args.length<=0){
			testMap.makeTestMap();
		} else {
			MapInterpreter interpreter = new MapInterpreter();
			interpreter.setMap(testMap);
			interpreter.interpret(args);
		}
		MapPanel view = new MapPanel();
		view.setPreferredSize(new Dimension (500,500));
		view.makeVisible(true);
		
		FullFrame frame = new FullFrame("MapPanel");

		frame.add(view,BorderLayout.CENTER);
		frame.setVisible(true);
		frame.pack();
		frame.validate();
	}
	
	public MapPanel(){
		super();
		this.add(mapArea,BorderLayout.CENTER);
		this.addMouseListener(this);
		this.addMouseMotionListener(this);
		this.addComponentListener(new ResizeListener());
	}
	
	public Map getMap(){return myMap;}
	public void setMap(Map map){
		Logger.log("MapPanel: set map " + map.getName());
		myMap = map;
	}
	
	public void makeVisible(boolean b){
		this.visible = b;
		this.redraw = true;
	}
	
	public void addNotify(){
		super.addNotify();
		startGame();
	}
	
	private void startGame(){
		if (animator == null || !running){
			animator = new Thread(this);
			animator.start();
		}
	}
	
	public void run(){
		long beforeTime, timeDiff, sleepTime;
		beforeTime = System.currentTimeMillis();
		running = true;
		while (running){
			//TODO could add pausing here - see page 37
			//gameUpdate();
			gameRender();
			
			timeDiff = System.currentTimeMillis() - beforeTime;
			sleepTime = period - timeDiff;
			if (sleepTime <=0){
				sleepTime = 5;
			}
			try{
				Thread.sleep(sleepTime);
			} catch (Exception e){}
			beforeTime = System.currentTimeMillis();
		}
	}
	
	// This is a simple render process that draws directly onto the canvas and does not 
	// use buffering
	private void gameRender(){
		//Logger.say("mapPanel.gameRender.visible " + visible);
		if (!visible ) return;

		Graphics g = this.getGraphics();
		drawMap(g);
		drawEntities(g);
	}
	
	private Image mapCanvas;
	
	private void drawMap(Graphics panelGraphics){
		if (mapCanvas == null) redraw = true;
		//Logger.log("drawing");
		int xoffset = 0;
		int yoffset = 0;
		if (redraw) {
			int panelWidth = this.getWidth()-2;
			if (panelWidth < 5) return;
			int panelHeight = this.getHeight()-2;
			if (panelHeight < 5) return;
			panelGraphics.setColor(Color.LIGHT_GRAY);
			panelGraphics.fillRect(0, 0, panelWidth, panelHeight);
			mapCanvas = createImage(panelWidth, panelHeight);
			Graphics mapGraphics = mapCanvas.getGraphics();
			mapGraphics.setColor(Color.WHITE);
			mapGraphics.fillRect(0, 0, panelWidth, panelHeight);
			mapGraphics.setColor(Color.BLACK);

			if (myMap == null) return;
			mapTransposer.update(myMap,panelWidth,panelHeight);
			if (!relief){
				drawAreas(mapGraphics);
			} else {
				drawRelief(mapGraphics);
			}
			drawBuildings(mapGraphics);
			drawRoads(mapGraphics);
			drawRivers(mapGraphics);
			if (relief){
			} else {
				//drawContours(mapGraphics); //TODO implement contours
			}
			drawBorder(mapGraphics); //FIXME should actually draw one pixel outside border
		}
		panelGraphics.drawImage(mapCanvas, 0, 0, this);
		redraw = false;
	}

	private void drawBorder(Graphics g){
		double mapx = myMap.getLL().getX();
		int x1 = mapTransposer.map2screenX(mapx);
		mapx = myMap.getUR().getX();
		int x2 = mapTransposer.map2screenX(mapx);
		int wide = x2 - x1;
		double mapy = myMap.getUR().getY();
		int y1 = mapTransposer.map2screenY(mapy);
		mapy = myMap.getLL().getY();
		int y2 = mapTransposer.map2screenY(mapy);
		int high = y2-y1;
//		high = -100;
		g.setColor(Color.black);
		g.drawRect(x1, y1, wide, high);
		
	}
	
	private void drawAreas(Graphics g){
		Iterator<Feature> areaIterator = myMap.getAreaIterator();
		while (areaIterator.hasNext()){
			Area area = (Area) areaIterator.next();
			Iterator<Coordinate> coordinateIterator = area.getCoordinateList().getIterator();
			Polygon p = makePolygon(coordinateIterator, area);

			if (p.npoints>0){
				g.setColor(area.getFeatureType().getColor());
				g.fillPolygon(p);
			}
		}
	}
	
	private void drawBuildings(Graphics g){
		Iterator<Feature> buildingIterator = myMap.getBuildingIterator();
		while (buildingIterator.hasNext()){
			Building building = (Building) buildingIterator.next();
			Iterator<Coordinate> coordinateIterator = building.getCoordinateList().getIterator();
			Polygon p = makePolygon(coordinateIterator, building);

			if (p.npoints>0){
				g.setColor(building.getFeatureType().getColor());
				g.fillPolygon(p);
			}
		}
	}
	
	private void drawRoads(Graphics g){
		Iterator<Feature> roadIterator = myMap.getRoadIterator();
		while (roadIterator.hasNext()){
			Road road = (Road) roadIterator.next();
			Iterator<Coordinate> coordinateIterator = road.getCoordinateList().getIterator();
			int arrayX[] = new int[road.getCoordinateList().getSize()];
			int arrayY[] = new int[road.getCoordinateList().getSize()];
			int coordinateCounter = 0;
			while (coordinateIterator.hasNext()){
				Coordinate c = coordinateIterator.next();
				double x = c.getX();
				arrayX[coordinateCounter] = mapTransposer.map2screenX(x);
				double y = c.getY();
				arrayY[coordinateCounter] = mapTransposer.map2screenY(y);
				coordinateCounter++;
			}

			if (coordinateCounter>0){
				g.setColor(road.getFeatureType().getColor());
				g.drawPolyline(arrayX, arrayY, coordinateCounter);
			}
		}
	}
	
	private void drawRivers(Graphics g){
		Iterator<Feature> riverIterator = myMap.getRiverIterator();
		while (riverIterator.hasNext()){
			River river = (River) riverIterator.next();
			Iterator<Coordinate> coordinateIterator = river.getCoordinateList().getIterator();
			int arrayX[] = new int[river.getCoordinateList().getSize()];
			int arrayY[] = new int[river.getCoordinateList().getSize()];
			int coordinateCounter = 0;
			while (coordinateIterator.hasNext()){
				Coordinate c = coordinateIterator.next();
				double x = c.getX();
				arrayX[coordinateCounter] = mapTransposer.map2screenX(x);
				double y = c.getY();
				arrayY[coordinateCounter] = mapTransposer.map2screenY(y);
				coordinateCounter++;
			}

			if (coordinateCounter>0){
				g.setColor(river.getFeatureType().getColor());
				g.drawPolyline(arrayX, arrayY, coordinateCounter);
			}
		}
	}
	
	private Polygon makePolygon(Iterator<Coordinate> coordinateIterator, AreaFeature area){
		int arrayX[] = new int[area.getCoordinateList().getSize()];
		int arrayY[] = new int[area.getCoordinateList().getSize()];
		int coordinateCounter = 0;
		while (coordinateIterator.hasNext()){
			Coordinate c = coordinateIterator.next();
			double x = c.getX();
			arrayX[coordinateCounter] = mapTransposer.map2screenX(x);
			double y = c.getY();
			arrayY[coordinateCounter] = mapTransposer.map2screenY(y);
			coordinateCounter++;
		}
		Polygon p = new Polygon(arrayX, arrayY, coordinateCounter);
		return p;
	}

	public void setSpriteManager(SpriteManager manager){
		mySpriteManager = manager;
	}
	
	private void drawEntities(Graphics g){
		if (mySpriteManager == null) return;
		mySpriteManager.drawEntities(g, mapTransposer);
		return;
	}
	
	public void addMapListener(MapListener listener){
		if (listener == null) return;
		mapListeners.addElement(listener);
	}
	
	private void showCoordinate(Coordinate c){
		if (mapListeners.size()<=0){
			//System.out.println(x);
		} else {
			//System.out.println("sending message");
			for (int i=0;i<mapListeners.size();i++){
				mapListeners.elementAt(i).showCoordinate(c);
			}
		}
	}
	
	private void selectCoordinate(Coordinate c){
		if (mapListeners.size()<=0){
			Logger.say(getReport(c));
		} else {
			Logger.say(getReport(c)); // TODO remove
			for (int i=0;i<mapListeners.size();i++){
				mapListeners.elementAt(i).selectCoordinate(c);
			}
		}
	}
	
	private String getReport(Coordinate c){
		String message = "pressed ( " + String.format("%.5f", c.getX()) 
		+ " , " + String.format("%.5f", c.getY()) +")" ;
		message = message + "\nLL " + myMap.getLL().getX() + ":" + myMap.getLL().getY();
		message = message + "\ncell " + myMap.getCellX(c) + ":" + myMap.getCellY(c);
		message = message + "\nelevation " + myMap.getElevationM(c) +"m";
		AreaFeature feature = myMap.whichAreaFeature(c);
		if (feature != null){
			message = message + "\n" + feature.getFeatureType().getName();
		} else {
			message = message + " open" ;
		}
		return message;
	}
	
	
	/****
	 * Mouse listener functions
	 */

	@Override
	public void mousePressed(MouseEvent e) {
		int x = e.getX();
		int y = e.getY();
		Coordinate c = mapTransposer.screen2Map(x, y);
		this.selectCoordinate(c);
	}

	@Override
	public void mouseMoved(MouseEvent e) {
		int x = e.getX();
		int y = e.getY();
		Coordinate c = mapTransposer.screen2Map(x, y);
		this.showCoordinate(c);
	}

	@Override
	public void mouseReleased(MouseEvent e) {}
	@Override
	public void mouseDragged(MouseEvent e) {}
	@Override
	public void mouseClicked(MouseEvent e) {}
	@Override
	public void mouseEntered(MouseEvent e) {}
	@Override
	public void mouseExited(MouseEvent e) {}
	
	class ResizeListener extends ComponentAdapter {
        public void componentResized(ComponentEvent e) {
        	redraw = true;
        }
        public void componentShown(ComponentEvent e) {
        	redraw = true;
        }
        public void componentMoved(ComponentEvent e) {
        	redraw = true;
        }
	}

	private void drawContours(Graphics g){
		int interval = 100; // metres
		Logger.say("drawing contours");
		double min = this.myMap.getElevationModel().getLowestM();
		double max = this.myMap.getElevationModel().getHighestM();
		double diff = max-min;
		Logger.say("min " + min);
		Logger.say("max " + max);
		Logger.say("diff " + diff);
		Logger.say("interval " + interval);
		
		int startContour = (int) (interval * (1 + Math.floor(min / interval)));
		int endContour = (int) (interval * (Math.floor(max / interval)));
		Logger.say("start " + startContour);
		Logger.say("  end " + endContour);
		int contour = startContour;
		while (contour <= endContour){
			Logger.say("  doing " + contour);
			contour += interval;
		}
	}

	private void drawRelief(Graphics g){
		double resolution = 1.0;
		Logger.say("drawing relief");
		double min = this.myMap.getElevationModel().getLowestM();
		double max = this.myMap.getElevationModel().getHighestM();
		double diff = max-min;
		int granularity = 20;
		int gradient = 50/ granularity;
		double step = diff/ granularity;
		//int cellsWide = this.myMap.getElevationModel().getWidth();
		//int cellsHigh = this.myMap.getElevationModel().getHeight();
		int cellsWide = (int) (myMap.getWidth()/ resolution);
		int cellsHigh = (int) (myMap.getHeight()/ resolution);
		Logger.say("size " + cellsWide + ":" + cellsHigh);
		double y = myMap.getLL().getY();
		for (int j=0;j<cellsHigh-1;j++){
			double x = myMap.getLL().getX();
			for (int i=0;i<cellsWide;i++){
				Coordinate c = new Coordinate(x + resolution*0.5, y + resolution * 0.5);
				double elev = myMap.getElevationM(c);
				elev = elev - min;
				elev = elev / step;
				int shade = 200 + (gradient * ((int) elev));
				if ( shade > 13) {
				//Logger.say("shade " + shade);
				}
				g.setColor(new Color(shade, shade, shade));
				int x1 = mapTransposer.map2screenX(x);
				int y1 = mapTransposer.map2screenY(y);
				int x2 = mapTransposer.map2screenX(x+ resolution);
				int y2 = mapTransposer.map2screenY(y+ resolution);
				//Logger.say("" + x1 + ":" + y1);
				Polygon p = new Polygon();
				p.addPoint(x1, y1);
				p.addPoint(x1, y2);
				p.addPoint(x2, y2);
				p.addPoint(x2, y1);
				g.fillPolygon(p);
				x = x + resolution;
			}
			y = y + resolution;
		}
	}
}
