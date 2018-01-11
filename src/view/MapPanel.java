package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Toolkit;

import javax.swing.JPanel;

import data.map.Map;
import utils.Logger;

public class MapPanel extends JPanel implements Runnable{

	private Thread animator;
	private volatile boolean running = false;
	private volatile boolean gameOver = false;
	
	public static void main(String[] args){
		
		MapPanel view = new MapPanel();
		view.setMap(new Map());
		view.setPreferredSize(new Dimension (500,500));
		
		FullFrame frame = new FullFrame("MapPanel");

		frame.add(view,BorderLayout.CENTER);
		frame.setVisible(true);
		frame.pack();
		frame.validate();
		
		//TODO add mouselistener - see page 20
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
	
	private Map myMap;
	public void setMap (Map map){myMap = map;}
	public Map getMap(){return myMap;}
	
	private void calculateTranslation(){
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
	}
	
	private double viewOffsetX;
	private double viewOffsetY;
	private double ratioX = 1.0;
	private double ratioY = 1.0;
	private int screenOffsetX = 1;
	private int screenOffsetY = 1;
	
	private void renderMap(Graphics g){
		if (myMap == null) return;
		//Logger.say("valid map");
		calculateTranslation();
		drawBorder(g);
//		int xpoints[] = {1, width-2, width-2, 1};
//	    int ypoints[] = {1, 1, height-1, height-1};
//	    int npoints = 4;
	    
//	    g.drawPolygon(xpoints, ypoints, npoints);
	}
	
	private void drawBorder(Graphics g){
		double mapx = myMap.getLL().getX();
		int x1 = map2screenX(mapx);
		mapx = myMap.getUR().getX();
		int x2 = map2screenX(mapx);
		int wide = x2 - x1;
		double mapy = myMap.getUR().getY();
		int y1 = map2screenY(mapy);
		mapy = myMap.getLL().getY();
		int y2 = map2screenY(mapy);
		int high = y2-y1;
//		high = -100;
		g.setColor(Color.black);
		g.drawRect(x1, y1, wide, high);
		
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
