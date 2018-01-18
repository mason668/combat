package data.map;

import sim.Constants;

public class RoadType extends FeatureType{
	
	public RoadType (String name){
		super.setName(name);
		for (int i=0;i<Constants.MOVER_TYPES;i++){
			speedFactor[i] = 1.0;
		}
	}
	
	public double getSpeedReduction (int moverType){ // TODO validate mover type
		if (moverType >= Constants.MOVER_TYPES) return 0.0;
		if (moverType <0) return 0.0;
		return speedFactor[moverType];
	}
	
	public static final int PRIMARY_ROAD = 1;
	public static final int SECONDARY_ROAD = 2;
	private int roadClass = PRIMARY_ROAD; // globtrrn.kroadclas
	private double width = 4.0; // globtrrn.roadwide

	private double[] speedFactor = new double[Constants.MOVER_TYPES]; // kroaddgrad 

}
