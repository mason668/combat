package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Polygon;
import java.awt.Toolkit;
import java.util.Iterator;

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

public class MapPanel extends JPanel implements Runnable{

	private Thread animator;
	private volatile boolean running = false;
	private volatile boolean gameOver = false;
	
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
		view.setMap(testMap);
		view.setPreferredSize(new Dimension (500,500));
		
		FullFrame frame = new FullFrame("MapPanel");

		frame.add(view,BorderLayout.CENTER);
		frame.setVisible(true);
		frame.pack();
		frame.validate();
		
		//TODO add mouselistener - see page 20
	}
	
	public void setMap(Map map){myMap = map;}
	
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
	
//	private final long period = 1000; // miliseconds
	private final long period = 1000;
	
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
	
	// This is a simple render process that draws dircetly onto the canvas and does not 
	// use buffering
	//TODO replace this with the double buffer versions - at some point
	private void gameRender(){
		Graphics g = this.getGraphics();
		drawMap(g);
	}
	
	private void drawMap(Graphics g){
		//Logger.log("drawing");
		int width = this.getWidth()-1;
		int height = this.getHeight()-1;
		g.setColor(Color.WHITE);
		g.fillRect(0, 0, width, height);
		g.setColor(Color.BLACK);
//		g.drawRect(1, 0, width-2, height-1);
//		g.drawRect(100, 100, 50, -50);
//		int xpoints[] = {1, width-2, width-2, 1};
//	    int ypoints[] = {1, 1, height-1, height-1};
//	    int npoints = 4;
	    
//	    g.drawPolygon(xpoints, ypoints, npoints);

		renderMap(g);
	}
	
	/*
	private int map2screenX(double x){
		double mapx = x;
		mapx = mapx - viewOffsetX;
		double screenX = mapx * ratioX;
//		System.out.println("calc " + viewOffsetX + " : " + mapx + " : " + screenX);
		screenX = screenX + screenOffsetX;
		int ix = (int) screenX;
		return ix;
	}
	
	private int map2screenY(double y){
		double mapy = y;
		mapy = mapy - viewOffsetY;
		double screenY = mapy * ratioY;
		screenY = screenOffsetY - screenY;
		System.out.println("calc " + viewOffsetY + " : " + mapy + " : " + screenY);
		int iy = (int) screenY;
		return iy;
	}
	private int screen2MapX(int x){return 0;}
	private int screen2MapY(int x){return 0;}
	*/
	
	private Map myMap;
	public Map getMap(){return myMap;}
	
	private MapTransposer mapTransposer;
	
	private void calculateTranslation(){
		mapTransposer = new MapTransposer(myMap,this.getWidth()-3,this.getHeight()-4);
		/*
		int panelWidth = this.getWidth()-3;
		int panelHeight = this.getHeight()-4;
		int screenWidth = panelWidth; //TODO adjust to centre image
		int screenHeight = panelHeight;
		double mapWidth = myMap.getSizeX();
		double mapHeight = myMap.getSizeY();
		double viewWidth = mapWidth;// TODO should account for zooming
		double viewHeight = mapHeight;
		double mapXll = myMap.getLL().getX();
		double mapYll = myMap.getLL().getY();
		double mapXur = myMap.getUR().getX();
		double mapYur = myMap.getUR().getY();
		double viewXll = mapXll;
		double viewYll = mapYll;
		double viewXur = mapXur;
		double viewYur = mapYur;
		
		screenOffsetX = 1;
		screenOffsetY = screenHeight;
		
		if (screenWidth < screenHeight){
			//
		} else {
			//
		}
		double screenXll = 1;
		double screenYll = screenHeight;
		double screenXur = screenWidth;
		double screenYur = 1;
		
		viewOffsetX = viewXll;
		viewOffsetY = viewYll;
		ratioX = screenWidth/ viewWidth;
//		System.out.println("ratio " + ratioX + " : " + viewWidth + " : " + screenWidth);
		ratioY = screenHeight/ viewHeight;
		System.out.println("ratio " + ratioY + " : " + viewHeight + " : " + screenHeight);
		*/
	}
	
//	private double viewOffsetX;
//	private double viewOffsetY;
//	private double ratioX = 1.0;
//	private double ratioY = 1.0;
//	private int screenOffsetX = 1;
//	private int screenOffsetY = 1;
	
	private void renderMap(Graphics g){
		if (myMap == null) return;
		//Logger.say("valid map");
		calculateTranslation();
		drawBorder(g);
		drawAreas(g);
		drawBuildings(g);
		drawRoads(g);
		drawRivers(g);
//		int xpoints[] = {1, width-2, width-2, 1};
//	    int ypoints[] = {1, 1, height-1, height-1};
//	    int npoints = 4;
	    
//	    g.drawPolygon(xpoints, ypoints, npoints);
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
		Iterator<Area> areaIterator = myMap.getAreaIterator();
		int count = 0;
		while (areaIterator.hasNext()){
			count++;
			Area area = areaIterator.next();
			Iterator<Coordinate> coordinateIterator = area.getCoordinates().iterator();
			Polygon p = makePolygon(coordinateIterator, area);

			if (p.npoints>0){
				//Logger.say("drawing " + count + " : " + p.npoints);
				g.setColor(area.getType().getColor());
				g.fillPolygon(p);
			}
		}
	}
	
	private void drawBuildings(Graphics g){
		Iterator<Building> buildingIterator = myMap.getBuildingIterator();
		int count = 0;
		while (buildingIterator.hasNext()){
			count++;
			Building building = buildingIterator.next();
			Iterator<Coordinate> coordinateIterator = building.getCoordinates().iterator();
			Polygon p = makePolygon(coordinateIterator, building);

			if (p.npoints>0){
				//Logger.say("drawing " + count + " : " + p.npoints);
				g.setColor(building.getType().getColor());
				g.fillPolygon(p);
			}
		}
	}
	
	private void drawRoads(Graphics g){
		Iterator<Road> roadIterator = myMap.getRoadIterator();
		int count = 0;
		while (roadIterator.hasNext()){
			count++;
			Road road = roadIterator.next();
			Iterator<Coordinate> coordinateIterator = road.getCoordinates().iterator();
			int arrayX[] = new int[road.getCoordinates().size()];
			int arrayY[] = new int[road.getCoordinates().size()];
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
				//Logger.say("drawing " + count + " : " + coordinateCounter);
				g.setColor(road.getType().getColor());
				g.drawPolyline(arrayX, arrayY, coordinateCounter);
			}
		}
	}
	
	private void drawRivers(Graphics g){
		Iterator<River> riverIterator = myMap.getRiverIterator();
		int count = 0;
		while (riverIterator.hasNext()){
			count++;
			River river = riverIterator.next();
			Iterator<Coordinate> coordinateIterator = river.getCoordinates().iterator();
			int arrayX[] = new int[river.getCoordinates().size()];
			int arrayY[] = new int[river.getCoordinates().size()];
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
				//Logger.say("drawing " + count + " : " + coordinateCounter);
				g.setColor(river.getType().getColor());
				g.drawPolyline(arrayX, arrayY, coordinateCounter);
			}
		}
	}
	
	private Polygon makePolygon(Iterator<Coordinate> coordinateIterator, Feature area){
		int arrayX[] = new int[area.getCoordinates().size()];
		int arrayY[] = new int[area.getCoordinates().size()];
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

	private Polygon makePolygon(Iterator<Coordinate> coordinateIterator, AreaFeature area){
		int arrayX[] = new int[area.getCoordinates().size()];
		int arrayY[] = new int[area.getCoordinates().size()];
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

	private Graphics graphics=null;
	private Image image = null;
	private void gameRender2(){ // this is supposed to implement double buffering
		int width = this.getWidth();
		int height = this.getHeight();
		if (width<= 0) return;
		if (height<=0) return;
		if (image == null){
			image = createImage(width, height);
			if (image == null){
				Logger.say("huh");
				Logger.log(Logger.WARNING,"dbImage = null");
				return;
			}
			graphics = image.getGraphics();
		} else {
		}
		if (graphics == null){
			image = null;
			return;
		}
		
		drawMap(graphics);
		
		paintScreen();
		
	}

	private void paintScreen(){
		Graphics g;
		try{
			g = this.getGraphics();
			if ((g!= null) && (image != null)){
				g.drawImage(image,0,0,null);
			}
			Toolkit.getDefaultToolkit().sync();
			g.dispose();
		} catch (Exception e){
			// TODO display error message
		}
	}

}
