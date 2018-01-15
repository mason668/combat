package data.map;

import java.awt.Polygon;
import java.util.Iterator;
import java.util.Vector;

import utils.Logger;

public class Feature {
	
	protected Vector<Coordinate> coordinates = new Vector<Coordinate>();
	public Vector<Coordinate> getCoordinates(){return coordinates;}
	
	public void addNode(Coordinate node){
		coordinates.add(node);
	}
	public int getSize(){
		return coordinates.size();
	}
	
}
