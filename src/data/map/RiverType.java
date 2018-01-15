package data.map;

public class RiverType extends FeatureType{

	public RiverType (String name){
		super.setName(name);
	}
	
	public static final int PRIMARY_RIVER = 1;
	public static final int SECONDARY_RIVER = 2;
	private int riverClass = PRIMARY_RIVER; // globtrrn.kriverclass
	private double width = 2.0; // globtrrn.riverwide
	private double depth = 0.5; // globtrrn.riverdepth
	/*
	INTEGER*2	KRIVERDGRAD(NUMMOVTYPES,NUMRIVERTYPES) ! percentage speed degradation for fording

	INTEGER*2  KRIVERDLAY(NUMMOVTYPES,NUMRIVERTYPES)	! Crossing times (minutes) by
						!  mover type:  1=wheeled,
						!  2=tracked, 3=footed

	INTEGER*2 KSWIMRIVER(NUMRIVERTYPES)	! crossing time for swimmers
	 */
	
}
