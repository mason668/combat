package data.map;

/**
 * A class to manage area features such as areas of vegetation or lakes.
 *
 */
public class AreaFeature extends Feature {

	/**
	 * Constructor.
	 * @param type The feature type to create.
	 */
	public AreaFeature(FeatureType type){
		super(type);
	}

	/**
	 * Determine if a point/ coordinate is within the area of the feature.
	 * @param point A coordinate to test.
	 * @return true if the coordinate is inside the feature.
	 */
	public boolean contains(Coordinate point){
		if (point == null) return false;
		if (this.getCoordinateList().getMaxX() < point.getX()) return false;
		if (this.getCoordinateList().getMinX() > point.getX()) return false;
		if (this.getCoordinateList().getMaxY() < point.getY()) return false;
		if (this.getCoordinateList().getMinY() > point.getY()) return false;
		double pointx = point.getX();
		double pointy = point.getY();
		boolean inside = false;
		for (int i=0;i< coordinates.getSize();i++){
			int j = i+1;
			if (i==coordinates.getSize()-1 ) j = 0;
			Coordinate vertex1 = coordinates.getCoordinate(i);
			Coordinate vertex2 = coordinates.getCoordinate(j);
			double x1 = vertex1.getX();
			double y1 = vertex1.getY();
			double x2 = vertex2.getX();
			double y2 = vertex2.getY();
			if ((pointy > y1) && (pointy > y2)) continue;
			if ((pointy < y1) && (pointy < y2)) continue;
			if ((pointx < x1) || (pointx < x2)) inside = !inside;
		}
		return inside;
	}
}
