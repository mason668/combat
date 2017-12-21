package sim.entity;

import java.util.Vector;

import data.csd.Platform;
import data.csd.Sensor;
import data.map.Coordinate;
import sim.forces.Force;

public interface ObserverEntity {
	public boolean isTracing();
	public String getID();
	public Platform getPlatform();
	public Coordinate getLocation();
	public Sensor getCurrentSensor();
	public Sensor getLastSensor();
	public void setLastSensor(Sensor sensor);
	public Vector<EntityDetection> getDetectionList();
	public Force getForce();
	public Entity getSpotTarget();
	public int getDetectionLevel(DetectedEntity target);
	public void setTargetList(TargetList vector);
	public boolean isDead();
	public String getName();

}
