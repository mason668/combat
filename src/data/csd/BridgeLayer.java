package data.csd;

import sim.Constants;

public class BridgeLayer {

	// from globengc
	private int LimitVehicles = 1;
	private int limitFoot = 1;
	private double limitWeight = 100;
	private int length = 20; // m
	private double setupTime = 300.0; // seconds
	private double packupTime = 300; // seconds
	private int vulcat = 1;
	private double crossingTime[] = new double[Constants.MOVER_TYPES];
	
	public double getPackupTime(){return packupTime;}

}
