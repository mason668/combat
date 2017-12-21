package sim.entity;

import data.csd.Platform;
import data.map.Coordinate;
import sim.forces.Force;

public interface DetectedEntity {
	public String getName();
	public Coordinate getLocation();
	public int getNumberOfElements();
	public CarrierEntity getCarrier();
	public Force getForce();
	public boolean isDead();
	public Platform getPlatform();
	public double getApparentSize(double range);
	public double getRealSize();
	public int getContrastClass();
	public double getCurrentSpeed();

}
