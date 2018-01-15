package data.map;

import java.awt.Color;
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
	
	private String mapName = "test_map"; // globtrrn.terrainame$
	
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
	
	private RoadTypeList roadTypeList = new RoadTypeList();
	public void addRoadType(RoadType roadType){
		roadTypeList.add(roadType);
	}
	public RoadType getRoadType(String name){
		return roadTypeList.getRoadType(name);
	}
	private Vector<Road> roadList = new Vector<Road>();
	public void addRoad(Road item){
		roadList.add(item);
	}
	public int getNumRoads(){
		return roadList.size();
	}
	public Iterator<Road> getRoadIterator(){
		return roadList.iterator();
	}

	private RiverTypeList riverTypeList = new RiverTypeList();
	public void addRiverType(RiverType riverType){
		riverTypeList.add(riverType);
	}
	public RiverType getRiverType(String name){
		return riverTypeList.getRiverType(name);
	}
	private Vector<River> riverList = new Vector<River>();
	public void addRiver(River item){
		riverList.add(item);
	}
	public int getNumRiverss(){
		return riverList.size();
	}
	public Iterator<River> getRiverIterator(){
		return riverList.iterator();
	}
	
	private WallTypeList wallTypeList = new WallTypeList();
	public void addWallType(WallType wallType){
		wallTypeList.add(wallType);
	}
	public WallType getWallType(String name){
		return wallTypeList.getWallType(name);
	}
	private Vector<Wall> wallList = new Vector<Wall>();
	public void addWall(Wall item){
		wallList.add(item);
	}
	public int getNumWalls(){
		return wallList.size();
	}
	public Iterator<Wall> getWallIterator(){
		return wallList.iterator();
	}

	private BuildingTypeList buildingTypeList = new BuildingTypeList();
	public void addBuildingType(BuildingType buildingType){
		buildingTypeList.add(buildingType);
	}
	public BuildingType getBuildingType(String name){
		return buildingTypeList.getBuildingType(name);
	}
	private Vector<Building> buildingList = new Vector<Building>();
	public void addBuilding(Building item){
		buildingList.add(item);
	}
	public int getNumBuildings(){
		return buildingList.size();
	}
	public Iterator<Building> getBuildingIterator(){
		return buildingList.iterator();
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
		
		this.addAreaType(new AreaType("Light_veg"));
		this.addAreaType(new AreaType("Medium_veg"));
		this.addAreaType(new AreaType("Dense_veg"));
		this.addAreaType(new AreaType("Lake"));
		this.addRiverType(new RiverType("River"));
		this.addRiverType(new RiverType("Creek"));
		this.addRoadType(new RoadType("Highway"));
		this.addRoadType(new RoadType("Road"));
		this.addRoadType(new RoadType("Track"));
		this.addBuildingType(new BuildingType("Brick_Building"));
		this.addBuildingType(new BuildingType("Wood_Building"));
		this.addWallType(new WallType("Wall"));
		this.addWallType(new WallType("Fence"));
		this.addWallType(new WallType("Hedge"));

		//FIXME - don't use new color but just 3 values - as this validates it properly
		this.areaTypeList.getAreaType("Light_veg").setColor(new Color(145,240,70));
		this.areaTypeList.getAreaType("Medium_veg").setColor(new Color(70,180,70));
		this.areaTypeList.getAreaType("Dense_veg").setColor(new Color(35,95,35));
		this.areaTypeList.getAreaType("Lake").setColor(new Color(10,150,170));
		this.riverTypeList.getRiverType("River").setColor(new Color(10,150,170));
		this.riverTypeList.getRiverType("Creek").setColor(new Color(45,170,120));
		this.roadTypeList.getRoadType("Highway").setColor(new Color(190,55,40));
		this.roadTypeList.getRoadType("Road").setColor(new Color(250,250,30));
		this.roadTypeList.getRoadType("Track").setColor(new Color(180,10,35));
		this.buildingTypeList.getBuildingType("Brick_Building").setColor(new Color(240,135,130));
		this.buildingTypeList.getBuildingType("Wood_Building").setColor(new Color(10,10,10));
		this.wallTypeList.getWallType("Wall").setColor(new Color(10,10,10));
		this.wallTypeList.getWallType("Fence").setColor(new Color(90,90,90));
		this.wallTypeList.getWallType("Hedge").setColor(new Color(15,180,60));
		
		Area area = new Area(this.getAreaType("Light_veg"));
		area.addNode(new Coordinate(101.0,101.0));
		area.addNode(new Coordinate(102.0,101.0));
		area.addNode(new Coordinate(102.0,102.0));
		area.addNode(new Coordinate(101.0,102.0));
		this.addArea(area);

		area = new Area(this.getAreaType("Medium_veg"));
		area.addNode(new Coordinate(102.0,102.0));
		area.addNode(new Coordinate(103.0,102.0));
		area.addNode(new Coordinate(103.0,103.0));
		area.addNode(new Coordinate(102.0,103.0));
		this.addArea(area);
		
		area = new Area(this.getAreaType("Dense_veg"));
		area.addNode(new Coordinate(103.0,103.0));
		area.addNode(new Coordinate(104.0,103.0));
		area.addNode(new Coordinate(104.0,104.0));
		area.addNode(new Coordinate(103.0,104.0));
		this.addArea(area);
		
		area = new Area(this.getAreaType("Lake"));
		area.addNode(new Coordinate(104.0,104.0));
		area.addNode(new Coordinate(105.0,104.0));
		area.addNode(new Coordinate(105.0,105.0));
		area.addNode(new Coordinate(104.0,105.0));
		this.addArea(area);
		
		Building building = new Building(this.getBuildingType("Brick_Building"));
		building.addNode(new Coordinate(105.0,105.0));
		building.addNode(new Coordinate(106.0,105.0));
		building.addNode(new Coordinate(106.0,106.0));
		building.addNode(new Coordinate(105.0,106.0));
		this.addBuilding(building);

		Road road = new Road(this.getRoadType("Highway"));
		road.addNode(new Coordinate(106.0,106.0));
		road.addNode(new Coordinate(107.0,106.0));
		road.addNode(new Coordinate(107.0,107.0));
		road.addNode(new Coordinate(106.0,107.0));
		this.addRoad(road);

		River river = new River(this.getRiverType("River"));
		river.addNode(new Coordinate(107.0,107.0));
		river.addNode(new Coordinate(108.0,107.0));
		river.addNode(new Coordinate(108.0,108.0));
		river.addNode(new Coordinate(107.0,108.0));
		this.addRiver(river);


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
		Tracer.write("road types: " + this.roadTypeList.getSize());
		for (String s: this.roadTypeList.keySet()){
			RoadType roadType = this.roadTypeList.getRoadType(s);
			Tracer.write("    " + roadType.getName() + " : " + roadType.getColor() );
		}
		Tracer.write("river types: " + this.riverTypeList.getSize());
		for (String s: this.riverTypeList.keySet()){
			RiverType riverType = this.riverTypeList.getRiverType(s);
			Tracer.write("    " + riverType.getName() + " : " + riverType.getColor() );
		}
		Tracer.write("wall types: " + this.wallTypeList.getSize());
		for (String s: this.wallTypeList.keySet()){
			WallType wallType = this.wallTypeList.getWallType(s);
			Tracer.write("    " + wallType.getName() + " : " + wallType.getColor() );
		}
		Tracer.write("building types: " + this.buildingTypeList.getSize());
		for (String s: this.buildingTypeList.keySet()){
			BuildingType buildingType = this.buildingTypeList.getBuildingType(s);
			Tracer.write("    " + buildingType.getName() + " : " + buildingType.getColor() );
		}
		Tracer.write("wall types: " + this.wallTypeList.getSize());
		for (String s: this.wallTypeList.keySet()){
			WallType wallType = this.wallTypeList.getWallType(s);
			Tracer.write("    " + wallType.getName() + " : " + wallType.getColor() );
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
		
		Tracer.write("buildings: " + this.buildingList.size());
		Iterator<Building> buildingIterator = this.getBuildingIterator();
		count = 0;
		while (buildingIterator.hasNext()){
			count++;
			Building building = buildingIterator.next();
			Tracer.write("building " + count + " type: " + building.getType().getName());
			for (Coordinate c: building.getCoordinates()){
				Tracer.write("    " + c);
			}
		}

		Tracer.write("roads: " + this.roadList.size());
		Iterator<Road> roadIterator = this.getRoadIterator();
		count = 0;
		while (roadIterator.hasNext()){
			count++;
			Road road = roadIterator.next();
			Tracer.write("road " + count + " type: " + road.getType().getName());
			for (Coordinate c: road.getCoordinates()){
				Tracer.write("    " + c);
			}
		}

		Tracer.write("rivers: " + this.riverList.size());
		Iterator<River> riverIterator = this.getRiverIterator();
		count = 0;
		while (riverIterator.hasNext()){
			count++;
			River river = riverIterator.next();
			Tracer.write("river " + count + " type: " + river.getType().getName());
			for (Coordinate c: river.getCoordinates()){
				Tracer.write("    " + c);
			}
		}
	}
}
	
/*
 * 
C	Grid Cell data word, bit field definition:			C
C									C
C		Bits 0-15	Elevation (pentimeters)			C
C		Bit  16 	Building present			C
C		Bit  17		Fence present				C
C		Bit  18 	Road present				C
C		Bit  19 	River present				C
C		Bit  20 	Vegetation present			C
C		Bit  21		Urban area present			C
C		Bit  22		Generic String present			C
C		Bit  23		Generic Area present			C
C		Bit  24		Obstacle present			C
C		Bit  25		Minefield present			C
C		Bit  26		Breach Lane present			C
C		Bits 27-31	Not used				C
C									C
	INTEGER*4	MASKELEV, MASKBILD, MASKFENC, MASKROAD, MASKRIVER
	INTEGER*4	MASKTREE, MASKCITY, MASKSTR, MASKAREA, MASKOBS
	INTEGER*4	MASKMINE, MASKLANE, MASKNONE, MASKANY, MASKPOLY


	INTEGER*2  KRIVERXMIN(NUMFEATURES)
	INTEGER*2  KRIVERXMAX(NUMFEATURES)
	INTEGER*2  KRIVERYMIN(NUMFEATURES)
	INTEGER*2  KRIVERYMAX(NUMFEATURES)
	INTEGER*4  KNUMRIVERS

	REAL*4		RPOLYXMIN(NUMFEATURES)
	INTEGER*4	KPOLYXMIN(NUMFEATURES)
	INTEGER*4	KNUMPOLYS
	REAL*4		MAX_POLY_SIZE


	CHARACTER*3	UTM_ZONE$	! the 3 char utm zone for the LL of the map eg 49J
	CHARACTER*1	UTM_X$		! the alpha char for the X axis
	CHARACTER*1	UTM_Y$		!

	REAL		ZBX1, ZBX2, EZ, NZ, ZBM, ZBB

	INTEGER*4	FEATS_PER_CELL
	PARAMETER	(FEATS_PER_CELL = 20)

	INTEGER*4	CELL_FEATURES
	INTEGER*4	MAX_CELLS
	PARAMETER (MAX_CELLS = (MAPGRIDX/10)*(MAPGRIDX/10))

C-------------------------------------------------------------------------------

*/

