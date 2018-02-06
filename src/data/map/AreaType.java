package data.map;

import sim.Constants;

public class AreaType extends FeatureType{
	
	public AreaType (String name){
		super.setName(name);
		for (int i=0;i<Constants.MOVER_TYPES;i++){
			speedFactor[i] = 1.0;
		}
	}
	
	private double featureHeight = 1.0; // metres globtrrn.ktreehei, kcityhei
	private boolean isWater = false;
	// TODO is swimming different to fording? - maybe we need LandArea and WaterArea
	public void setWater(boolean b){isWater = b;}
	public boolean getWater(){
		return isWater;
	}
	
	public double getHeightM(){ // trrnlib.trrn_get_feature_height
		return featureHeight;
	}
	public void setHeightM(double height){
		if (height >= 0.0 ) featureHeight = height;
	}
	
	// fill pattern ktreepattern, citypattern
	
}
