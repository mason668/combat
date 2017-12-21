package sim.forces;

import java.util.HashMap;
import sim.Constants;
import sim.entity.DetectedEntity;
import sim.entity.EntityDetection;

public class Force {
	public Force (String s){
		name = s.substring(0);
	}
	
	private String name = "force";
	public String getName(){return name.substring(0);}
	
	private double speed = 20.0;
	public double getSpeed(){return speed;}
	public void setSpeed(double d){speed = d;}
	
	/** Each force has a set maximum number of targets each entity can have
	 * for each relationship type: friendly, enemy, neutral.
	 */
	private int numberOfTargets = 10;
	
	public int getNumberOfTargets(){
		return numberOfTargets;
	}
	public void setNumberOfTargets(int i){
		numberOfTargets = i; 
	}
	
	private HashMap<Integer, EntityDetection> detectionList = 
		new HashMap<Integer, EntityDetection>(); //TODO not sure if this is best
	public void updateDetection(EntityDetection detection){}; //TODO do something about this
	public int getDetectionLevel(DetectedEntity target){ //TODO is entity the right type?
		return Constants.OBSERVATION_LEVEL_DETECTED;
	}
	private double scanTime = 120.0; // default 2 mins
	public double getScanTime(){ return scanTime;}
	public void setScanTime(double time){
		if ( time <=0.0 ) return;
		scanTime = time;
	}
	private double searchTime = 5.0; // default 5 secs
	public double getSearchTime(){return searchTime;}
	public void setSearchTime(double time){
		if ( time <=0.0 ) return;
		searchTime = time;
	}

	private HashMap<String, Integer> hostilityMatrix = new HashMap<String,Integer>();

	public void setHostility(String forceName, int hostility){
		if (hostility < Constants.HOSTILITY_UNKNOWN) return;
		if (hostility > Constants.HOSTILITY_UNSEEN) return;
		hostilityMatrix.put(forceName, new Integer(hostility));
	}
	public int getHostility(String forceName){
		Integer i = hostilityMatrix.get(forceName);
		if (i!= null) return i.intValue();
		return Constants.HOSTILITY_UNKNOWN;
	}

}
