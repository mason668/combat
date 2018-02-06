package data.map;

import java.util.Iterator;
import java.util.Vector;

/**
 * A class to manage a list of coordinates.
 */
public class CoordinateList {

	/** 
	 * Store the list as a vector
	 */
	protected Vector<Coordinate> coordinateList = new Vector<Coordinate>();
	
	/**
	 * Remember the maximum x value among the coordinates
	 */
	protected double maxX = -9999.9999;
	
	/**
	 * Remember the maximum y value among the coordinates
	 */
	protected double maxY = -9999.9999;
	
	/**
	 * Remember the minimum x value among the coordinates 
	 */
	protected double minX = 9999.9999;
	
	/**
	 * Remember the minimum y value among the coordinates 
	 */
	protected double minY = 9999.9999;
	
	/**
	 * Add a Coordinate to the list
	 * @param coordinate The Coordinate to add.
	 */
	public void addCoordinate(Coordinate coordinate){
		if (coordinate == null) return;
		coordinateList.add(coordinate);
		if (coordinate.getX()< minX){
			minX = coordinate.getX();
		}
		if (coordinate.getX()> maxX){
			maxX = coordinate.getX();
		}
		if (coordinate.getY()< minY){
			minY = coordinate.getY();
		}
		if (coordinate.getY()> maxY){
			maxY = coordinate.getY();
		}
	}
	
	/**
	 * Get a specific coordinate from the list. 
	 * @param index The index of the coordinate in the ordered list starting with zero.
	 * @return The corresponding coordinate or null if there is no matching coordinate.
	 */
	public Coordinate getCoordinate(int index){
		if (index < coordinateList.size()){
			return coordinateList.get(index); //TODO could make a copy to preserve integrity
		}
		return null;
	}
	
	/**
	 * Get an Iterator for the list.
	 * @return An iterator.
	 */
	public Iterator<Coordinate> getIterator(){
		return coordinateList.iterator();
	}
	
	/**
	 * Get the maximum x value among all the coordinates in this feature.
	 * @return The maximum x.
	 */
	public double getMaxX(){
		return maxX;
	}

	/**
	 * Get the maximum y value among all the coordinates in this feature.
	 * @return The maximum y.
	 */
	public double getMaxY(){
		return maxY;
	}
	
	/**
	 * Get the minimum x value among all the coordinates in this feature.
	 * @return The minimum x.
	 */
	public double getMinX(){
		return minX;
	}
	
	/**
	 * Get the minimum y value among all the coordinates in this feature.
	 * @return The minimum y.
	 */
	public double getMinY(){
		return minY;
	}
	
	/**
	 * Get the size of the list.
	 * @return The number of coordinates in the list.
	 */
	public int getSize(){
		return coordinateList.size();
	}

	//TODO add findlowest/ highest etc move a coordinate etc
}
