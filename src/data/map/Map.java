package data.map;

import java.util.Iterator;
import java.util.Vector;

import utils.Tracer;

/**
 * A class that contains all of the data required to represent a map
 * and the simple functions that allow access to those data.
 *
 */
public class Map {
	
	/**
	 * The class is executable so a simple test can be run.
	 * The test creates a simple test terrain file and prints 
	 * a report of its data. 
	 * @param args At this point, args are ignored.
	 */
	public static void main(String[] args){
		Map me = new Map();
		me.makeTestMap();
		Tracer.setEcho(true);
		me.trace();
	}
	
	private String mapName = "test_map";
	
	public String getName(){return mapName.substring(0);}
	public void setName(String name){
		this.mapName = name.substring(0);
	}

	private double cellSize = 0.1;
	public double getCellSize(){return cellSize;}
	
	private double maxStepSize = 0.05;
	public double getMaxStepSize(){return maxStepSize;}
	public void setMaxStepSize(double d){
		if (d < 0.01) return;
		if (d> 5.0) return;
		maxStepSize = d;
	}

	private Coordinate lowerLeft = new Coordinate(100.0,100.0);
	private Coordinate upperRight = new Coordinate(110.0,110.0);
	
	public Coordinate getLL(){
		return lowerLeft;
	}
	public void setLowerLeft(Coordinate c){
		lowerLeft = new Coordinate(c);
	}
	public Coordinate getUR(){
		return upperRight;
	}
	public void setUpperRight(Coordinate c){
		upperRight = new Coordinate(c);
	}
	
	public void setXLL(double d){
		lowerLeft.setX(d);
	}
	public void setYLL(double d){
		lowerLeft.setY(d);
	}
	public void setXUR(double d){
		upperRight.setX(d);
	}
	public void setYUR(double d){
		upperRight.setY(d);
	}
	
	public double getSizeX(){
		return upperRight.getX() - lowerLeft.getX();
	}
	
	public double getSizeY(){
		return upperRight.getY() - lowerLeft.getY();
	}
	
	private AreaTypeList areaTypeList = new AreaTypeList();
	public void addAreaType(AreaType areaType){
		areaTypeList.add(areaType);
	}
	public AreaType getAreaType(String name){
		return areaTypeList.getAreaType(name);
	}
	
	private Vector<Area> areaList = new Vector<Area>();
	public void addArea(Area item){
		areaList.add(item);
	}
	public int getNumAreas(){
		return areaList.size();
	}
	public Iterator<Area> getAreaIterator(){
		return areaList.iterator();
	}

	// ***
	/*
	 * general purpose functions
	 */
	
	// determine if coordinate is on the map or not
	public boolean onMap(Coordinate c){
		return true;
	}
	
	public RoadType getRoadType(Coordinate c){return null;}
	public double getSlope(Coordinate c1, Coordinate c2){return 0.00;}
	public AreaType getAreaType(Coordinate c){return null;}
	public RiverType getRiverType(Coordinate c){ return null;}
	public double getAreaHeight(Coordinate c){return 0.001;}
	public boolean anyCraters(Coordinate c1, Coordinate c2){return false;}

	public double getElevation(Coordinate c){
		return 0.1;
	}
	public Building inBuilding(Coordinate c){
		return null;
	}
	
	public void makeTestMap(){
		lowerLeft = new Coordinate(100.0,100.0);
		upperRight = new Coordinate(110.0,110.0);
		
		this.addAreaType(new AreaType("light_veg"));
		this.addAreaType(new AreaType("medium_veg"));
		this.addAreaType(new AreaType("dense_veg"));
		
		Area area = new Area(this.getAreaType("light_veg"));
		this.addArea(area);
		area = new Area(this.getAreaType("medium_veg"));
		this.addArea(area);
		area = new Area(this.getAreaType("dense_veg"));
		this.addArea(area);
		
		area.addNode(new Coordinate(101.0,101.0));
		area.addNode(new Coordinate(102.0,101.0));
		area.addNode(new Coordinate(102.0,102.0));
		area.addNode(new Coordinate(101.0,102.0));

	}
	
	
	public void trace(){
		Tracer.write("report for map " + this.getName());
		Tracer.write("lower left " + lowerLeft.toString());
		Tracer.write("upper right " + upperRight.toString());
		Tracer.write("width " + this.getSizeX());
		Tracer.write("height " + this.getSizeY());
		Tracer.write("area types: " + this.areaTypeList.getSize());
		for (String s: this.areaTypeList.keySet()){
			AreaType areaType = this.areaTypeList.getAreaType(s);
			Tracer.write("    " + areaType.getName() + " : " + areaType.getColor());
		}
		Tracer.write("areas: " + this.areaList.size());
		Iterator<Area> iterator = this.getAreaIterator();
		int count = 0;
		while (iterator.hasNext()){
			count++;
			Area area = iterator.next();
			Tracer.write("area " + count + " type: " + area.getType().getName());
			for (Coordinate c: area.getCoordinates()){
				Tracer.write("    " + c);
			}
		}
	}
	
}
