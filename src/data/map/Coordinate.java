package data.map;

public class Coordinate {
	
	public static final double EAST = 0.0;
	public static final double WEST = Math.PI;
	public static final double SOUTH = WEST + (Math.PI * 0.5);
	public static final double NORTH = EAST + (Math.PI * 0.5);
	
	private double myx = -1;
	private double myy = -1;
	private double myz = -1;
	
	public Coordinate (double x, double y){
		myx = x;
		myy = y;
		myz = 0;
	}
	
	public Coordinate (double x, double y, double z){
		myx = x;
		myy = y;
		myz = z;
	}
	
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
	
	public double getX(){return myx;}
	public void setX(double d){myx = d;}
	public double getY(){return myy;}
	public void setY(double d){myy = d;}
	public double getZ(){return myz;}
	public void setZ(double d){myz = d;}
	
	public static double distance2D(Coordinate c1, Coordinate c2){
		return Math.sqrt(distanceSquare2D(c1, c2));
	}
	
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