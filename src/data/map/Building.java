package data.map;

public class Building extends AreaFeature{
	
	public Building(BuildingType type){
		super(type);
	}
	
	public int getFloors(){return 3;}
	public double getHeight(){return 0.02;}

	/*
	 * 	INTEGER*2	KNUMFLOORS (NUMBUILDINGS)	! Number of floors.
	REAL*4		BUILDING_HEIGHT(NUMBUILDINGS)
	REAL	   BILDHEIGHT(NUMBILDTYPES)	! Total height (meters).

	REAL	   BILDOPENING(NUMBILDTYPES)	! Fractional area of exterior
						!  wall openings.

	REAL	   BILDFORTTIM(4,NUMBILDTYPES)	! Engineer minutes to reach
						!  fortification levels 1-4.

        REAL       BILDGRADFACT(NUMBILDTYPES)   ! Building Rubbling Factor

	INTEGER*2  KBILDROOMS  (NUMBILDTYPES)	! Total # of rooms.

	BYTE	   KBILDCONTYP (NUMBILDTYPES)	! Construction type.

	BYTE	   KBILDFLOORS (NUMBILDTYPES)	! Number of floors.

	BYTE	   KBILDCLAS   (NUMBILDTYPES)	! Functional classification.

	INTEGER*4  	KBILDXMIN(NUMBUILDINGS)
	REAL*4		RBILDXMIN(NUMBUILDINGS)

C	INTEGER*2  KBILDXMAX(NUMFEATURES)
C	INTEGER*2  KBILDYMIN(NUMFEATURES)
C	INTEGER*2  KBILDYMAX(NUMFEATURES)

	REAL*4		MAX_BUILD_SIZE

	 */
}
