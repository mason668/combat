package sim.entity;

import data.map.Coordinate;

public interface CarrierEntity {
	public String getName();
	public boolean isDead();
	public void setDelay(double time);
	public Coordinate getLocation();

}
