package data.map;

public class RoadType {
	public String getName(){
		return "road";
	}
	public double getSpeedReduction (int moverType){ // TODO validate mover type
		return 1.0;
	}

}
