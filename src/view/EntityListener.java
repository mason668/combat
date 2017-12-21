package view;

import data.map.Coordinate;
import sim.entity.Entity;

public interface EntityListener {
	
	public void updateLocation (Entity entity, Coordinate location);

}
