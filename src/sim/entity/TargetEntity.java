package sim.entity;

import java.util.Vector;

import data.csd.Platform;
import data.map.Coordinate;

public interface TargetEntity {

	public String getName();
	public Platform getPlatform();
	public int getVulnerability();
	public int getNumberOfElements();
	public Vector<Coordinate> getElementLocations();
	public double getDirectionFace();
	public double getCurrentSpeed();
	public int getDefilade();
	public Coordinate getLocation();
	public CarrierEntity getCarrier();
}
