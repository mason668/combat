package sim.entity;

import data.map.Coordinate;

public interface EntityListener {
	
	public void updateLocation (Entity entity, Coordinate location);

}
