package data.map;

import sim.Constants;

public class RiverType extends FeatureType{

	public RiverType (String name){
		super.setName(name);
		for (int i=0;i<Constants.MOVER_TYPES;i++){
			crossingTime[i] = 5.0; // minutes
			speedFactor[i] = 0.5;
		}
	}
	
	public static final int PRIMARY_RIVER = 1;
	public static final int SECONDARY_RIVER = 2;
	private int riverClass = PRIMARY_RIVER; // globtrrn.kriverclass
	private double width = 2.0; // globtrrn.riverwide
	private double depth = 0.5; // globtrrn.riverdepth
	private double[] crossingTime = new double[Constants.MOVER_TYPES]; // kriverdlay 
	private double[] speedFactor = new double[Constants.MOVER_TYPES]; // kriverdgrad 
	private double swimTime = 0.5; // crossing time for swimmers minutes kswimriver

}
