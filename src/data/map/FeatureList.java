package data.map;

import java.util.Iterator;
import java.util.Vector;

/**
 * A class to maintain a list of Features
 *
 */
public class FeatureList {
	
	/**
	 * Store the list as a vector
	 */
	private Vector<Feature> featureList = new Vector<Feature>();

	/**
	 * Add a Feature to the list.
	 * @param feature The Feature to add.
	 */
	public void add(Feature feature){
		if (feature == null) return;
		featureList.add(feature);
	}

	/**
	 * Get an iterator for the list.
	 * @return An iterator.
	 */
	public Iterator<Feature> getIterator(){
		return featureList.iterator();
	}
	
	/**
	 * Get the number of features in the list.
	 * @return The number of features.
	 */
	public int size(){
		return featureList.size();
	}

	/**
	 * Determine which feature in the list contains a specific coordinate.
	 * @param coordinate The coordinate to test.
	 * @return The first feature found that contains the coordinate or null if none are found.
	 */
	public AreaFeature whichAreaFeature(Coordinate coordinate){ // trrn_which_poly
		Iterator<Feature> iterator = this.getIterator();
		while (iterator.hasNext()){
			AreaFeature feature = (AreaFeature) iterator.next();
			FeatureType type = feature.getFeatureType();
			if (feature.contains(coordinate)){
				return feature;
			}
		}
		return null; // TODO
		/*
	IDATA = ITRRNDATA ( X,Y)

	IF ( (IDATA .AND. MASKPOLY) .EQ. 0) THEN
	  GOTO 999
	ENDIF

	CALL POLYS_BETWEEN_X (X, X, IFIRST_POLY, ILAST_POLY)

	IF ( IFIRST_POLY .GT. 0 ) THEN
	  IPOLYS = ILAST_POLY - IFIRST_POLY +1
	ELSE
	  GOTO 999
	ENDIF

	DO INDEX = IFIRST_POLY, ILAST_POLY
	  IFEAT = KPOLYXMIN(INDEX)

	  CALL TRRN_TEST_POLY  ( X,Y, IFEAT )

	  IF( IFEAT .GT. 0 )  THEN
	    GOTO 999
	  ENDIF

100	CONTINUE
	ENDDO

	IFEAT = 0

999	RETURN
	END


		 * 
		 */

	}


}
