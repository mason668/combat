package data.map;

public class RoadType extends FeatureType{
	
	public RoadType (String name){
		super.setName(name);
	}
	
	public double getSpeedReduction (int moverType){ // TODO validate mover type
		return 1.0;
	}
	
	public static final int PRIMARY_ROAD = 1;
	public static final int SECONDARY_ROAD = 2;
	private int roadClass = PRIMARY_ROAD; // globtrrn.kroadclas
	private double width = 4.0; // globtrrn.roadwide
	
	/*
	 * 
	BYTE	   KROADDGRAD(NUMMOVTYPES,0:NUMROADTYPES) ! Speed degradation factors
						!  (percent), by mover type:
						! 1=wheeled, 2=tracked, 3=footed
	 */

}
