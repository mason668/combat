package data.map;

public class AreaType extends FeatureType{
	
	public AreaType (String name){
		super.setName(name);
	}
	
	/*
	 * 
	BYTE	   KTREEDGRAD(NUMMOVTYPES,NUMTREETYPES)	! Speed degradation factors
						!  (percent), by mover type:
					        ! 1=wheeled, 2=tracked, 3=footed
	BYTE	   KCITYDGRAD(NUMMOVTYPES,NUMCITYTYPES)	! Speed degradation factors
						!  (percent), by mover type:
					        ! 1=wheeled, 2=tracked, 3=footed
	 */
	
	public double getSpeedReduction (int moverType){ // TODO validate mover type
		// ktreedgrad, kcitydgrad
		return 1.0;
	}
	
	private boolean isWater = false;
	
	public boolean isWater(){
		return isWater;
	}
	
	private double featureHeight = 1.0; // metres globtrrn.ktreehei, kcityhei
	private double plos = 1.0; // globtrrn.treeplos, cityplos
	// fill pattern ktreepattern, citypattern
	
}
