package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Toolkit;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

import data.map.Map;
import interpreter.MapInterpreter;
import utils.Logger;

public class MapView extends JPanel implements Runnable{
	private static final long serialVersionUID = 1L;
	private JPanel mapArea = new JPanel();
//	private JScrollPane mapPane = new JScrollPane(mapArea);
	private JPanel zoomArea = new JPanel();
	private JPanel menuArea = new JPanel();
	private Map myMap;
	
	private Thread animator;
	private volatile boolean running = false;
	private volatile boolean gameOver = false;

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
		
		//TODO add mouselistener - see page 20
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
		// assumes that dbg has been defined as Graphics
		screenx = mapArea.getWidth();
		screeny = mapArea.getHeight();
		Logger.say("size " + screenx + " : " + screeny);
		dbg.setColor(Color.BLACK);
		dbg.drawLine(0, 0, screenx, screeny);
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
	
	private final long period = 20;
	
	public void run(){
		long beforeTime, timeDiff, sleepTime;
		beforeTime = System.currentTimeMillis();
		running = true;
		while (running){
			//TODO could add pausing here - see page 37
			gameUpdate();
			gameRender();
			paintScreen();
			
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
	
	private void gameUpdate(){
		if (!gameOver){
			// update
		}
	}
	
	public void stopGmae(){
		running = false;
	}
	
	private Graphics dbg=null;
	private Image dbImage = null;
	
	private void gameRender(){
		int width = this.mapArea.getWidth();
		int height = this.mapArea.getHeight();
		if (width<= 0) return;
		if (height<=0) return;
		if (dbImage == null){
			dbImage = createImage(width, height);
			if (dbImage == null){
				Logger.say("huh");
				Logger.log(Logger.WARNING,"dbImage = null");
				return;
			}
			dbg = dbImage.getGraphics();
		} else {
		}
		if (dbg == null){
			dbImage = null;
			return;
		}
		dbg.setColor(Color.RED);
		dbg.fillRect(0, 0, width, height);
		
		drawMap();
		
		if (gameOver){
			// display game over message
		}
	}
	
	public void paintComponent(Graphics g){
		super.paintComponent(g);
		if (dbImage != null){
			g.drawImage(dbImage,0,0,null);
		}
	}
	
	private void paintScreen(){
		Graphics g;
		try{
			g = this.getGraphics();
			if ((g!= null) && (dbImage != null)){
				g.drawImage(dbImage,0,0,null);
			}
			Toolkit.getDefaultToolkit().sync();
			g.dispose();
		} catch (Exception e){
			// TODO display error message
		}
	}
	
}
