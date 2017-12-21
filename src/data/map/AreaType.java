package data.map;

public class AreaType {
	public double getSpeedReduction (int moverType){ // TODO validate mover type
		return 1.0;
	}
	public String getName(){
		return "generic area";
	}
	
	public boolean isWater(){
		return false;
	}

}
