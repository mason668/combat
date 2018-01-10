package data.map;

public class Map {
	
	private String mapName = "test_map";
	public String getName(){return mapName.substring(0);}
	public void setName(String name){
		this.mapName = name.substring(0);
	}

	public double getElevation(Coordinate c){
		return 0.1;
	}
	public Building inBuilding(Coordinate c){
		return null;
	}
	public double getCellSize(){return 0.1;}
	public double getMaxStepSize(){return 0.05;}
	
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

	private Coordinate lowerLeft = new Coordinate(0.0,0.0);
	private Coordinate upperRight = new Coordinate(10.0,10.0);
	public Coordinate getLL(){
		return lowerLeft;
	}
	public Coordinate getUR(){
		return upperRight;
	}
	public double getSizeX(){
		return 10.0;
	}
	public double getSIzeY(){
		return 10.0;
	}

}
