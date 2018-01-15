package data.map;

import java.awt.Polygon;
import java.util.Iterator;
import java.util.Vector;

import utils.Logger;

public class Area {
	
	private AreaType areaType;
	
	public Area(AreaType type){
		areaType = type; //TODO do we need to copy?
	}
	public AreaType getType(){
		return areaType;
	}
	
	private Vector<Coordinate> coordinates = new Vector<Coordinate>();
	public Vector<Coordinate> getCoordinates(){return coordinates;}
	
	public void addNode(Coordinate node){
		coordinates.add(node);
	}
	public int getSize(){
		return coordinates.size();
	}
	
	public Polygon getPolygon(MapTransposer mapTransposer){
		int arrayX[] = new int[coordinates.size()];
		int arrayY[] = new int[coordinates.size()];
		Iterator<Coordinate> itr = coordinates.iterator();
		int count = 0;
		while (itr.hasNext()){
			Coordinate c = itr.next();
			double x = c.getX();
			arrayX[count] = mapTransposer.map2screenX(x);
			double y = c.getY();
			arrayY[count] = mapTransposer.map2screenY(y);
			count++;
		}
		Polygon p = new Polygon(arrayX, arrayY, count);
		return p;
	}

}
