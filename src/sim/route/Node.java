package sim.route;

import data.map.Coordinate;
import sim.Scenario;
import sim.entity.Entity;
import sim.entity.MoverEntity;

public class Node {
	
	protected Coordinate location;
	public Coordinate getLocation(){return location;}
	public void setLocation(Coordinate c){
		location = new Coordinate(c);
	}
	
	public void doActivity(MoverEntity entity, Scenario scenario){
	}
	public boolean waiting(MoverEntity entity, Scenario scenario){
		return false;
	}
}
