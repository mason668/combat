package data.map;

import sim.Constants;

public class AreaType extends FeatureType{
	
	public AreaType (String name){
		super.setName(name);
		for (int i=0;i<Constants.MOVER_TYPES;i++){
			speedFactor[i] = 1.0;
		}
	}
	
	// TODO is swimming different to fording? - maybe we need LandArea and WaterArea
	private double[] speedFactor = new double[Constants.MOVER_TYPES]; // ktreedgrad, kcitydgrad 
	
	public double getSpeedReduction (int moverType){
		if (moverType >= Constants.MOVER_TYPES) return 0.0;
		if (moverType <0) return 0.0;
		return speedFactor[moverType];
	}
	public void setSpeedReduction(int moverType, double d){
		if (moverType >= Constants.MOVER_TYPES) return;
		if (moverType <0) return;
		if (d < 0.0) return;
		if (d > 1.0) return;
		speedFactor[moverType] = d;
	}

	private boolean isWater = false;
	public void setWater(boolean b){isWater = b;}
	public boolean getWater(){
		return isWater;
	}
	
	private double featureHeight = 1.0; // metres globtrrn.ktreehei, kcityhei
	public double getHeight(){return featureHeight;}
	public void setHeight(double height){
		if (height >= 0.0 ) featureHeight = height;
	}
	
	private double plos = 1.0; // globtrrn.treeplos, cityplos
	public double getPLOS(){return plos;}
	public void setPLOS(double p) {
		if (p < 0.0) return;
		if (p > 1.0) return;
		plos = p;
	}
	// fill pattern ktreepattern, citypattern
	
}
