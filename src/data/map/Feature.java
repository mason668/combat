package data.map;

import java.util.Vector;

public class Feature {
	
	protected Vector<Coordinate> coordinates = new Vector<Coordinate>();
	public Vector<Coordinate> getCoordinates(){return coordinates;}
	
	public void addNode(Coordinate node){
		coordinates.add(node);
	}
	public int getSize(){
		return coordinates.size();
	}
	
	protected double xmin, xmax, ymin, ymax;
	
}
