package data.map;

public class Wall extends LinearFeature{
	public Wall(WallType type){
		super();
		wallType = type;
	}
	
	private WallType wallType;
	public WallType getType(){
		return wallType;
	}
	

}
