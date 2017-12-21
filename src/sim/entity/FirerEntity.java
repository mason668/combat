package sim.entity;

import data.csd.Platform;
import data.map.Coordinate;

public interface FirerEntity {
	public String getName();
	public Platform getPlatform();
	public boolean isSuppressed();
	public boolean isTracing();
	public boolean getMOPP();
	public double getCurrentSpeed();
	public Coordinate getLocation();

}
