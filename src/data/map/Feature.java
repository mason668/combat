package data.map;

/**
 * A class to hold feature data such as roads, rivers and vegetation.
 */
public class Feature {
	/**
	 * Coordinates defining the shape of the feature
	 */
	protected CoordinateList coordinates = new CoordinateList();
	
	/**
	 * The type of feature. That is whether it is a specific road type or vegetation type etc.
	 */
	protected FeatureType myFeatureType;

	/**
	 * Constructor.
	 * @param type Specify the feature type.
	 */
	public Feature(FeatureType type){
		myFeatureType = type;
	}
	
	/**
	 * Get the feature type for this feature.
	 * @return The feature type.
	 */
	public FeatureType getFeatureType(){
		return myFeatureType;
	}

	/**
	 * Get the list of coordinates for the feature.
	 * @return The list of coordinates.
	 */
	public CoordinateList getCoordinateList(){
		return coordinates;
	}

}
