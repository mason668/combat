package data.map;

/**
 * A class to represent the 3 dimensional coordinate system. The units of measurement are 
 * unspecified, but are assumed to be consistent across all coordinates.
 *
 */
public class Coordinate {
	
	// define the cardinal directions
	public static final double EAST = 0.0;
	public static final double WEST = Math.PI;
	public static final double SOUTH = WEST + (Math.PI * 0.5);
	public static final double NORTH = EAST + (Math.PI * 0.5);
	
	private double myx = -1;
	private double myy = -1;
	private double myz = -1;

	/**
	 * Construct a Coordinate by defining the two primary dimensions - x and y.
	 * Z is set to zero.
	 * @param x
	 * @param y
	 */
	public Coordinate (double x, double y){
		x = Math.max(0, x);
		y = Math.max(0, y);
		myx = x;
		myy = y;
		myz = 0;
	}
	
	/**
	 * Construct a Coordinate by defining all three dimensions - x, y and z.
	 * @param x
	 * @param y
	 * @param z
	 */
	public Coordinate (double x, double y, double z){
		x = Math.max(0, x);
		y = Math.max(0, y);
		myx = x;
		myy = y;
		myz = z;
	}
	
	/**
	 * Construct a Coordinate by copying an existing coordinate.
	 * @param c The coordinate to copy.
	 */
	public Coordinate (Coordinate c){
		if ( c== null) return;
		myx = c.getX();
		myy = c.getY();
		myz = c.getZ();
	}
	
	public String toString(){
		String s = "(" + myx + "," + myy + "," + myz + ")";
		return s;
	}
	
	/**
	 * Get the x value of the coordinate. 
	 * @return The x value.
	 */
	public double getX(){return myx;}
	
	/**
	 * Set the x value of the coordinate.
	 * @param d the value to set.
	 */
	public void setX(double d){myx = d;}

	/**
	 * Get the y value of the coordinate. 
	 * @return The y value.
	 */
	public double getY(){return myy;}
	
	/**
	 * Set the y value of the coordinate.
	 * @param d the value to set.
	 */
	public void setY(double d){myy = d;}
	
	/**
	 * Get the z value of the coordinate. 
	 * @return The z value.
	 */
	public double getZ(){return myz;}
	
	/**
	 * Set the z value of the coordinate.
	 * @param d the value to set.
	 */
	public void setZ(double d){myz = d;}

	/**
	 * Calculate the absolute distance between 2 coordinates in the horizontal plane only. 
	 * @param c1 The first coordinate.
	 * @param c2 The second coordinate.
	 * @return the distance in the same units as the coordinate.
	 */
	public static double distance2D(Coordinate c1, Coordinate c2){
		return Math.sqrt(distanceSquare2D(c1, c2));
	}
	
	/**
	 * Calculate the square of the distance between 2 coordinates 
	 * in the horizontal plane only. 
	 * @param c1 The first coordinate.
	 * @param c2 The second coordinate.
	 * @return The square of the distance distance in the same units as the coordinate.
	 */
	public static double distanceSquare2D(Coordinate c1, Coordinate c2){
		double x1 = c1.getX();
		double y1 = c1.getY();
		double x2 = c2.getX();
		double y2 = c2.getY();
		double distx = x1-x2;
		double disty = y1-y2;
		return (distx*distx)+(disty*disty);
	}
	
	//TODO do 3d versions of distance

	/**
	 * Calculate the direction from coordinate 1 to coordinate 2. The directions 
	 * is in radians counter-clockwise from east. 
	 * @param c1
	 * @param c2
	 * @return The direction in radians from coordinate 1 to coordinate 2.
	 */
	public static double direction( Coordinate c1, Coordinate c2){
		double x1 = c1.getX();
		double y1 = c1.getY();
		double x2 = c2.getX();
		double y2 = c2.getY();
		double distx = x2-x1;
		double disty = y2-y1;
		if ( distx != 0 && disty !=0 ){
			double d = Math.atan2(disty, distx);
			while (d < 0){
				d = d + 2*Math.PI;
			}
			return d;
		} else if (distx == 0.0){
			if (disty < 0.0) {
				return SOUTH;
			} else {
				return NORTH;
			}
		} else {
			if (distx < 0.0) {
				return WEST;
			} else {
				return EAST;
			}
		}
	}
}