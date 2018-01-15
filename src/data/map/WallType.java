package data.map;

public class WallType extends FeatureType{
	
	public WallType (String name){
		super.setName(name);
	}
	
	/*
	BYTE	   KSTRHEI(NUMSTRTYPES)		! Height (in meters).
	REAL	   STRWIDE(NUMSTRTYPES)		! Half-width (in Kilometers).
	REAL	   STRPLOS(NUMSTRTYPES)		! PLOS (right angle)
	BYTE	   KSTRBRE			! For each Engineer Type:
     *		   (0:NUMBRECH,NUMSTRTYPES)	!   0 = cross feature
						!   1 = clear feature
	INTEGER*2  KSTRDLAY			! For each Engineer Type:
     *		   (0:NUMBRECH,NUMSTRTYPES)	!  time (minutes) to cross
						!  or clear feature.
	REAL*4		RSTRXMIN(NUMFEATURES)
	INTEGER*4	KSTRXMIN(NUMFEATURES)
	REAL*4		MAX_STRING_SIZE
	INTEGER*4	KNUMSTRINGS
		REAL	   FENHEI (NUMFENTYPES)		! Total height (meters).
	REAL	   FENANGL(NUMFENTYPES)		! Opaque angle (radians)
	REAL	   FENPLOS(NUMFENTYPES)		! PLOS at right angle

	BYTE	   KFENBRE			! For each Engineer Type:
     *		   (0:NUMBRECH,NUMFENTYPES)	!   0 = cross fence
						!   1 = clear fence

	INTEGER*2  KFENDLAY			! For each Engineer Type:
     *		   (0:NUMBRECH,NUMFENTYPES)	!  time (minutes) to cross
						!  or clear fence.
	 */
	
}
