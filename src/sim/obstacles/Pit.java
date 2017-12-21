package sim.obstacles;

import data.map.Coordinate;
import sim.entity.Entity;

public class Pit {
	private Coordinate myLocation;
	private Entity myEntity;
	
	public Coordinate getLocation(){
		return new Coordinate (myLocation);
	}
	public void setLocation(Coordinate location){
		if (location != null){
			myLocation = new Coordinate(location);
		}
	}
	
	public Entity getOccupant(){
		return myEntity;
	}
	public void setOccupant(Entity entity){
		myEntity = entity;
	}

}
