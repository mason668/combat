package data.map;

import java.awt.Color;
import java.util.Iterator;

import interpreter.MapInterpreter;
import utils.Tracer;

/**
 * A class that contains all of the data required to represent a map
 * and the simple functions that allow access to those data.
 *
 */
public class Map {
	
	/**
	 * The class is executable so a simple test can be run.
	 * The test creates a simple test terrain file and prints 
	 * a report of its data. 
	 * @param args At this point, args are ignored.
	 */
	public static void main(String[] args){
		//TODO implement args to change test functionality
		Map me = new Map();
		me.makeTestMap();
		if (args.length>0){
			MapInterpreter interpreter = new MapInterpreter();
			interpreter.setMap(me);
			interpreter.interpret(args);
		}
		Tracer.setEcho(true);
		me.trace();
	}
	
	/**
	 * Private members
	 */
	
	private ElevationModel elevationModel = new ElevationModel(100);
	private FeatureList areaList = new FeatureList();
	private FeatureTypeList areaTypeList = new FeatureTypeList();
	private FeatureList buildingList = new FeatureList();
	private FeatureTypeList buildingTypeList = new FeatureTypeList();
	private double cellSize = 0.1; //FIXME how do we define cell size?
	private Coordinate lowerLeft = new Coordinate(100.0,100.0);
	private String mapName = "test_map"; // globtrrn.terrainame$
	private double maxStepSize = 0.05;
	private FeatureList roadList = new FeatureList();
	private FeatureTypeList roadTypeList = new FeatureTypeList();
	private FeatureList riverList = new FeatureList();
	private FeatureTypeList riverTypeList = new FeatureTypeList();
	private Coordinate upperRight = new Coordinate(110.0,110.0);
	private FeatureList wallList = new FeatureList();
	private FeatureTypeList wallTypeList = new FeatureTypeList();
	private double windDirection = 0.0;
	private double windSpeed = 0.0;

	public void createElevationModel(int x, int y){
		if ( x < 1) return; //TODO should do some error reporting
		if ( y < 1) return;
		elevationModel = new ElevationModel(x,y);
	}
	public ElevationModel getElevationModel(){
		return elevationModel; // FIXME really should not expose this
	}
	
	public double getCellSize(){
		return cellSize;
	}
	public Coordinate getLL(){
		return lowerLeft;
	}
	public double getHeight(){
		return upperRight.getY() - lowerLeft.getY();
	}
	public double getFeatureHeightM(Coordinate c){
		return 0.0; // TODO
		/*
	REAL FUNCTION TRRNVEGH ( X,Y)
	! returns height of feature type in metres

	include 	'global.f'
	include 	'globtrrn.f'
	include		'globscr.f'

	TRRNVEGH = 0.0

        IROADTYP  =  INROAD( X,Y )
	IF ( IROADTYP .GT. 0 ) THEN
	  RETURN
	ENDIF

        IRIVERTYP  =  INRIVER( X,Y )
	IF ( IRIVERTYP .GT. 0 ) THEN
	  RETURN
	ENDIF

	IVEG = ITRRNSURF(X,Y)

	K = 0
	IF (IVEG .EQ. 20) THEN
	  K = 0
	ELSEIF ( IVEG .GT. 60 ) THEN
	  K = 0
	ELSEIF ( IVEG .GT. 40 ) THEN
	  K = KAREAHEI(IVEG-40)
	ELSEIF (IVEG .GT. 20 ) THEN
	  K = KTREEHEI(IVEG-20)
	ELSEIF ( IVEG .GT. 0 ) THEN
	  K = KCITYHEI(IVEG)
	ENDIF

	TRRNVEGH = FLOAT(K)

999	RETURN
	END

		 * 
		 */
	}
	public String getFeatureName(Coordinate c){
		return ""; // TODO
		/*
	SUBROUTINE TRN_NAME (X,Y,NAME$)

	include		'glbparam.f'
	include		'globtrrn.f'

	CHARACTER*20	NAME$

        IROADTYP  =  INROAD( X,Y )
	IF ( IROADTYP .GT. 0 ) THEN
	  NAME$ = ROADNAME$(IROADTYP)
	  RETURN
	ENDIF

        IRIVERTYP  =  INRIVER( X,Y )
	IF ( IRIVERTYP .GT. 0 ) THEN
	  NAME$ = RIVERNAME$(IRIVERTYP)
	  RETURN
	ENDIF

	IVEG = ITRRNSURF(X,Y)

	IF (IVEG .EQ. 20 ) THEN
	  NAME$ = 'Bare'
	ELSEIF ( IVEG .GT. 60 ) THEN
	  NAME$ = RIVERNAME$(IVEG-60)
	ELSEIF ( IVEG .GT. 40 ) THEN
	  NAME$ = AREANAME$(IVEG-40)
	ELSEIF (IVEG .GT. 20 ) THEN
	  NAME$ = TREENAME$(IVEG-20)
	ELSEIF ( IVEG .GT. 0 ) THEN
	  NAME$ = CITYNAME$(IVEG)
	ELSE
	  NAME$ = 'Bare'
	ENDIF

	CALL PAD(NAME$)

	RETURN
	END

		 * 
		 */
	}
	public double getMaxStepSize(){
		return maxStepSize;
	}
	public String getName(){
		return mapName.substring(0);
	}
	
	public void setMaxStepSize(double d){
		if (d < 0.01) return;
		if (d> 5.0) return;
		maxStepSize = d;
	}
	public Coordinate getUR(){
		return upperRight;
	}
	public double getWidth(){
		return upperRight.getX() - lowerLeft.getX();
	}
	public double getWindDirection(){ // trrnlib.trrnwind
		return windDirection;
	}
	public double getWindSpeed(){ // trrnlib.trrnwind
		return windSpeed;
	}

	public void setLowerLeft(Coordinate c){
		lowerLeft = new Coordinate(c);
	}
	public void setName(String name){
		this.mapName = name.substring(0);
	}

	
	public void setUpperRight(Coordinate c){
		upperRight = new Coordinate(c);
	}
	
	public void setXLL(double d){
		lowerLeft.setX(d);
	}
	public void setXUR(double d){
		upperRight.setX(d);
	}
	public void setYLL(double d){
		lowerLeft.setY(d);
	}
	public void setYUR(double d){
		upperRight.setY(d);
	}
	
	// TODO How to we track numroads etc?
	
	/*
	 * Data for buildings 
	 */
	
	public void addBuildingType(BuildingType buildingType){
		buildingTypeList.add(buildingType);
	}
	public BuildingType getBuildingType(String name){
		return (BuildingType) buildingTypeList.getFeatureType(name);
	}
	public void addBuilding(Building item){
		buildingList.add(item);
	}
	public int getNumBuildings(){
		return buildingList.size();
	}
	public Iterator<Feature> getBuildingIterator(){
		return buildingList.getIterator();
	}
	
	/*
	 * Data for areas
	 */
	
	public void addAreaType(AreaType areaType){
		areaTypeList.add(areaType);
	}
	public AreaType getAreaType(String name){
		return (AreaType) areaTypeList.getFeatureType(name);
	}
	public void addArea(Area item){
		areaList.add(item);
	}
	public int getNumAreas(){
		return areaList.size();
	}
	public Iterator<Feature> getAreaIterator(){
		return areaList.getIterator();
	}
	
	/*
	 * Data for rivers
	 */

	public void addRiverType(RiverType riverType){
		riverTypeList.add(riverType);
	}
	public RiverType getRiverType(String name){
		return (RiverType) riverTypeList.getFeatureType(name);
	}
	public void addRiver(River item){
		riverList.add(item);
	}
	public int getNumRiverss(){
		return riverList.size();
	}
	public double getPLOS(Coordinate c){
		return 1.0; // TODO
		/*
	REAL FUNCTION TRRNLOS ( X,Y)

	include 	'global.f'
	include 	'globtrrn.f'
	include		'globscr.f'

	IF (INROAD(X,Y) .GT. 0 ) THEN
	  TRRNLOS = 1.0
	  RETURN
	ENDIF

	ASIZE = GRIDSIZE * 1000	! Convert to metres
	TRRNLOS = 0.0
	IVEG = ITRRNSURF(X, Y)

	ALOS = 1.0
	IF (IVEG .EQ. 20 ) THEN
	  ALOS = 1.0
	ELSEIF ( IVEG .GT. 60 ) THEN
	  ALOS = 1.0
	ELSEIF ( IVEG .GT. 40 ) THEN
	  ALOS = AREAPLOS(IVEG-40)
	ELSEIF (IVEG .GT. 20 ) THEN
	  ALOS = TREEPLOS(IVEG-20)
	ELSEIF ( IVEG .GT. 0 ) THEN
	  ALOS = CITYPLOS(IVEG)
	ENDIF

c	IF ( ASIZE .LT. 100 ) THEN
c	  ALOS = 1.0 - ( (1.0 - ALOS ) / ( 100.0/ASIZE) )
c	ELSE
c	  ALOS = ( ALOS ) * ( 100.0/ASIZE)
c	ENDIF

	TRRNLOS = ALOS

999	RETURN
	END

		 * 
		 */
	}
	public double getPLOS(Coordinate c1, Coordinate c2){
		return 1.0; // TODO
		/*
	SUBROUTINE TRRNPLOS ( X,Y, HEIGHT, ALOS)

	include 	'global.f'
	include 	'globtrrn.f'
	include		'globscr.f'

	ASIZE = GRIDSIZE * 1000	! Convert to metres

	IF (INROAD(X,Y) .GT. 0 ) THEN
	  ALOS = 1.0
	  HEIGHT = 0
	  RETURN
	ENDIF

	K = 0
	HEIGHT = 0.0
	ALOS = 0.0
	IVEG = ITRRNSURF(X,Y)

	ALOS = 1.0
	IF (IVEG .EQ. 20 ) THEN
	  ALOS = 1.0
	  K = 0
	ELSEIF ( IVEG .GT. 60 ) THEN
	  ALOS = 1.0
	  K = 0
	ELSEIF ( IVEG .GT. 40 ) THEN
	  ALOS = AREAPLOS_STEP(IVEG-40)
	  K = KAREAHEI(IVEG-40)
	ELSEIF (IVEG .GT. 20 ) THEN
	  ALOS = TREEPLOS_STEP(IVEG-20)
	  K = KTREEHEI(IVEG-20)
	ELSEIF ( IVEG .GT. 0 ) THEN
	  ALOS = CITYPLOS_STEP(IVEG)
	  K = KCITYHEI(IVEG)
	ENDIF

	HEIGHT = FLOAT(K)

c	IF ( ASIZE .NE. 100.0) THEN
c	  RATIO = ASIZE * 0.01
c	  ALOS = ALOS ** RATIO
c	ENDIF

999	RETURN
	END

	SUBROUTINE TRRNPLOS_100 ( X,Y, HEIGHT, ALOS)

	include 	'global.f'
	include 	'globtrrn.f'
	include		'globscr.f'

	ASIZE = GRIDSIZE * 1000	! Convert to metres

	IF (INROAD(X,Y) .GT. 0 ) THEN
	  ALOS = 1.0
	  HEIGHT = 0
	  RETURN
	ENDIF

	K = 0
	HEIGHT = 0.0
	ALOS = 0.0
	IVEG = ITRRNSURF(X,Y)

	ALOS = 1.0
	IF (IVEG .EQ. 20 ) THEN
	  ALOS = 1.0
	  K = 0
	ELSEIF ( IVEG .GT. 60 ) THEN
	  ALOS = 1.0
	  K = 0
	ELSEIF ( IVEG .GT. 40 ) THEN
	  ALOS = AREAPLOS(IVEG-40)
	  K = KAREAHEI(IVEG-40)
	ELSEIF (IVEG .GT. 20 ) THEN
	  ALOS = TREEPLOS(IVEG-20)
	  K = KTREEHEI(IVEG-20)
	ELSEIF ( IVEG .GT. 0 ) THEN
	  ALOS = CITYPLOS(IVEG)
	  K = KCITYHEI(IVEG)
	ENDIF

	HEIGHT = FLOAT(K)

c	IF ( ASIZE .NE. 100.0) THEN
c	  RATIO = ASIZE * 0.01
c	  ALOS = ALOS ** RATIO
c	ENDIF

999	RETURN
	END

		 * 
		 */
	}
	public Iterator<Feature> getRiverIterator(){
		return riverList.getIterator();
	}
	
	/*
	 * Data for roads
	 */
	
	public void addRoadType(RoadType roadType){
		roadTypeList.add(roadType);
	}
	public RoadType getRoadType(String name){
		return (RoadType) roadTypeList.getFeatureType(name);
	}
	public void addRoad(Road item){
		roadList.add(item);
	}
	public int getNumRoads(){
		return roadList.size();
	}
	public Iterator<Feature> getRoadIterator(){
		return roadList.getIterator();
	}
	
	/*
	 * Data for walls and fences
	 */
	
	public void addWallType(WallType wallType){
		wallTypeList.add(wallType);
	}
	public WallType getWallType(String name){
		return (WallType) wallTypeList.getFeatureType(name);
	}
	public void addWall(Wall item){
		wallList.add(item);
	}
	public int getNumWalls(){
		return wallList.size();
	}
	public Iterator<Feature> getWallIterator(){
		return wallList.getIterator();
	}

	
	/*
		INTEGER*2  KRIVERXMIN(NUMFEATURES)
		INTEGER*2  KRIVERXMAX(NUMFEATURES)
		INTEGER*2  KRIVERYMIN(NUMFEATURES)
		INTEGER*2  KRIVERYMAX(NUMFEATURES)
		INTEGER*4  KNUMRIVERS

		REAL*4		RPOLYXMIN(NUMFEATURES)
		INTEGER*4	KPOLYXMIN(NUMFEATURES)
		INTEGER*4	KNUMPOLYS
		REAL*4		MAX_POLY_SIZE


		CHARACTER*3	UTM_ZONE$	! the 3 char utm zone for the LL of the map eg 49J
		CHARACTER*1	UTM_X$		! the alpha char for the X axis
		CHARACTER*1	UTM_Y$		!

		REAL		ZBX1, ZBX2, EZ, NZ, ZBM, ZBB

		INTEGER*4	FEATS_PER_CELL
		PARAMETER	(FEATS_PER_CELL = 20)

		INTEGER*4	CELL_FEATURES
		INTEGER*4	MAX_CELLS
		PARAMETER (MAX_CELLS = (MAPGRIDX/10)*(MAPGRIDX/10))

	C-------------------------------------------------------------------------------

	*/



	// ***
	/*
	 * general purpose functions
	 */
	
	
	public boolean anyCraters(Coordinate c1, Coordinate c2){return false;}
	public double getAreaHeight(Coordinate c){return 0.001;}
	public AreaType getAreaType(Coordinate c){return null;}
	public double getElevationKM(Coordinate c){
		return 0.001; // TODO
		/*
	REAL FUNCTION TRRNALT ( X,Y)	!returns km

	ELEV = TRRNELEV ( X,Y)	! returns metres
	TRRNALT = ELEV * 0.001

	RETURN
	END
		 * 
		 */
	}
	public int getCellX(Coordinate c){
		double x = c.getX();
		x = x - this.lowerLeft.getX();
		x = x / cellSize;
		int cellx = (int) x;
		return cellx;
	}
	public int getCellY(Coordinate c){
		double y = c.getY();
		y = y - this.lowerLeft.getY();
		y = y / cellSize;
		int celly = (int) y;
		return celly;
	}
	public double getElevationM(Coordinate c){
		int cellx = this.getCellX(c);
		int celly = this.getCellY(c);
		double elevation = this.elevationModel.getElevationM(cellx, celly);
		return elevation; // TODO this needs to interpolate
		/*
	REAL FUNCTION TRRNELEV ( X,Y)	! returns metres

	include 	'global.f'
	include 	'globtrrn.f'
	include		'globscr.f'

	IELEV = ITRRNDATA(X,Y)
	IELEV = IELEV .AND. MASKELEV
	TRRNELEV =  FLOAT ( IELEV ) / 5

	RETURN
	END
		 * 
		 */
	}
	public RiverType getRiverType(Coordinate c){ return null;}
	public RoadType getRoadType(Coordinate c){return null;}
	public double getSlope(Coordinate c1, Coordinate c2){
		return 0.00; //TODO
		/*
	SUBROUTINE TRRN_SLOPE (X1, Y1, X2, Y2, SLOPE, IDEBUG )

	include 'global.f'
	include 'globtrrn.f'
	include 'globrpt.f'

	DX = X1-X2
	DY = Y1-Y2
	DIST = SQRT ((DX*DX ) + ( DY*DY))
	DIST = DIST * 1000

	CALL TRRNELEV2 ( X1,Y1, ELEV1 )
	CALL TRRNELEV2 ( X2,Y2, ELEV2 )

D	IF ( IDEBUG .GT. 0 ) THEN
D	  WRITE (KLINE$,"('DIST = ',F12.3)") DIST
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"('ELEV1 = ',F12.3)") ELEV1
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"('ELEV2 = ',F12.3)") ELEV2
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	SLOPE  =  ABS( ELEV1 - ELEV2 ) /  DIST

D	IF ( IDEBUG .GT. 0 ) THEN
D	  WRITE (KLINE$,"('SLOPE = ',F12.3)") SLOPE
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	RETURN
	END

		 * 
		 */
	}
	public Building inBuilding(Coordinate c){
		return null;
	}
	// determine if coordinate is on the map or not
	public boolean onMap(Coordinate c){
		return true; // TODO
		/*
		 * 
	INTEGER*4 FUNCTION ITRRNTEST ( X,Y)

	include 	'global.f'
	include 	'globtrrn.f'
C	include		'globscr.f'

	ITRRNTEST = 0
	IF (X .LT. XLL ) ITRRNTEST = 1
	IF (X .GT. XLL+XWIDE ) ITRRNTEST = 1
	IF (Y .LT. YLL ) ITRRNTEST = 1
	IF (Y .GT. YLL+XWIDE ) ITRRNTEST = 1

999	RETURN
	END


		 */
	}
	public String getZone(){ // trrnlib.trrnzone
		//TODO
		return "";
		/*
	SUBROUTINE TRRNZONE ( A$ )

	include 	'global.f'
	include 	'globtrrn.f'

	CHARACTER*3 A$

	A$ = UTM_ZONE$

	RETURN
	END


		 * 
		 */
	}
	
	public AreaFeature whichAreaFeature(Coordinate c){ // trrn_which_poly
		return areaList.whichAreaFeature(c);
	}
	
	public LinearFeature whichLinearFeature (Coordinate c){
		return null;// TODO
		/*
	SUBROUTINE TRRN_WHICH_STRING ( X,Y, IFEAT)

	include 	'global.f'
	include 	'globtrrn.f'
	include		'globscr.f'

	IFEAT = 0

	IDATA = ITRRNDATA ( X,Y)

	IF ( (IDATA .AND. MASKSTR) .EQ. 0) THEN
	  GOTO 999
	ENDIF

	CALL STRINGS_BETWEEN_X (X, X, IFIRST_POLY, ILAST_POLY)

	IF ( IFIRST_POLY .GT. 0 ) THEN
	  IPOLYS = ILAST_POLY - IFIRST_POLY +1
	ELSE
	  GOTO 999
	ENDIF

	DO INDEX = IFIRST_POLY, ILAST_POLY
	  IFEAT = KSTRXMIN(INDEX)
	  KFTYPE = KTRNFTYPE(IFEAT)
	  KSTYPE = KTRNFSUBT(IFEAT)

	  IF( X .LT. TRNFXMIN(IFEAT) )  GOTO 200
	  IF( X .GT. TRNFXMAX(IFEAT) )  GOTO 200
	  IF( Y .LT. TRNFYMIN(IFEAT) )  GOTO 200
	  IF( Y .GT. TRNFYMAX(IFEAT) )  GOTO 200

C --------- Fetch this Feature's parameters.

	  NODE1  =  KTRNFPNTR(IFEAT)			! First node
	  NUMNODES  =  KTRNFNODES(IFEAT)			! Number of nodes

C --------- Is the point inside the polygon.

	  HALFWIDTH  =  STRWIDE(KSTYPE)

C --------- Skip if point not within ROAD's bounding box.

	  IF( X .LT. TRNFXMIN(IFEAT)-HALFWIDTH  )  GOTO 200
	  IF( X .GT. TRNFXMAX(IFEAT)+HALFWIDTH  )  GOTO 200
	  IF( Y .LT. TRNFYMIN(IFEAT)-HALFWIDTH  )  GOTO 200
	  IF( Y .GT. TRNFYMAX(IFEAT)+HALFWIDTH  )  GOTO 200

C --------- Setup to process this ROAD feature further.

	  HWSQRD     =  HALFWIDTH * HALFWIDTH		! Square of road's
							! half-width (km)

	  NODE1  =  KTRNFPNTR(IFEAT)			! First node
	  LAST   =  NODE1 + KTRNFNODES(IFEAT) - 1		! Last node

C --------- Fetch first node's coordinates

	  X2  =  TRNODESX(NODE1)
	  Y2  =  TRNODESY(NODE1)

C --------- Loop over the segments comprising this road STRING.

	  DO J = NODE1+1, LAST

C ------------- Set "previous" end-point

	    X1  =  X2
	    Y1  =  Y2

C ------------- Fetch "next" end-point of string segment

	    X2  =  TRNODESX(J)
	    Y2  =  TRNODESY(J)

C ------------- Skip if point not within segment's bounding box.

	    IF( X .LT. MIN( X1,X2 )-HALFWIDTH  )  GOTO 100
	    IF( X .GT. MAX( X1,X2 )+HALFWIDTH  )  GOTO 100
	    IF( Y .LT. MIN( Y1,Y2 )-HALFWIDTH  )  GOTO 100
	    IF( Y .GT. MAX( Y1,Y2 )+HALFWIDTH  )  GOTO 100

C ------------- Test distance from point to segment's centerline.

	    CALL SQDISPTL ( X,Y, X1,Y1, X2,Y2, DSQRD )

	    IF( DSQRD .LE. HWSQRD )  THEN
	      GOTO 999
	    ENDIF

100	  ENDDO
200	ENDDO

	IFEAT = 0

999	RETURN
	END


		 * 
		 */
	}
	
	/*
	 * 

	SUBROUTINE TRRN_COORDS (X, Y, XW, YT)

C 	This routine will fetch the terrain characteristics from the
C 	database

	include 	'global.f'
	include 	'globtrrn.f'

	X = XLL
	Y = YLL
	XW = XLL + XWIDE
	YT = YLL + XWIDE

	RETURN
	END

	INTEGER*4 FUNCTION ITRRNCELL ( X,Y )

	include 	'global.f'
	include 	'globtrrn.f'

	XCELL  =  1.0 + ( (X-XLL) / GRIDSIZE )
	YCELL  =  1.0 + ( (Y-YLL) / GRIDSIZE )
	IXG  =  XCELL
	IYG  =  YCELL

	IF (IXG .LE. 0 .OR. IXG .GT. KDIMX .OR.
     *		IYG .LE. 0 .OR. IYG .GT. KDIMX ) THEN
	  PRINT *,'ERROR - ITRRNCELL: Invalid value.'
	  PRINT *,X, Y, XLL, YLL, XLL+XWIDE, YLL+XWIDE, GRIDSIZE
	  ICELL = -1
	  GOTO 999
	ENDIF

	ICELL   =  IXG  +  (IYG-1) * KDIMX

999	CONTINUE
	ITRRNCELL = ICELL
	RETURN
	END

	SUBROUTINE TRRNCELL ( X,Y, ICELL )

	include 	'global.f'
	include 	'globtrrn.f'

	ICELL = ITRRNCELL (X,Y)

999	RETURN
	END

	INTEGER*4 FUNCTION ITRRNSURF ( X,Y)

	! returns a number from 0 - 15 to indicate veg type

	INTEGER*4	IVEG

	include 	'global.f'
	include 	'globtrrn.f'
	include		'globscr.f'

	ITRRNSURF = 0

	CALL TRRN_WHICH_POLY ( X,Y, IFEAT)

	IF ( IFEAT .LE. 0 ) THEN
	  GOTO 999
	ENDIF

	KFTYPE  =  KTRNFTYPE(IFEAT)			! feature's Sub-Type
	ITYP  =  KTRNFSUBT(IFEAT)			! feature's Sub-Type

	IF ( KFTYPE .EQ. FEAT_RIVER ) THEN
	  ITYP = ITYP + 60
	ELSEIF ( KFTYPE .EQ. FEAT_VEG ) THEN
	  ITYP = ITYP + 20
	ELSEIF ( KFTYPE .EQ. FEAT_URBAN ) THEN
	  ITYP = ITYP
	ELSEIF ( KFTYPE .EQ. FEAT_AREA ) THEN
	    ITYP = 40 + ITYP
	ENDIF

	ITRRNSURF = ITYP

C	PARAMETER ( FEAT_BLANK  = 1)
C	PARAMETER ( FEAT_FENCE  = 2)
C	PARAMETER ( FEAT_ROAD   = 3)
C	PARAMETER ( FEAT_RIVER  = 4)
C	PARAMETER ( FEAT_VEG    = 5)
C	PARAMETER ( FEAT_URBAN  = 6)
C	PARAMETER ( FEAT_STRING = 7)
C	PARAMETER ( FEAT_AREA   = 8)


999	RETURN
	END

	INTEGER*4 FUNCTION ITRRNRUFF ( X,Y)

	INTEGER*4	I

	include 	'global.f'
	include 	'globtrrn.f'
	include		'globscr.f'

	ITRRNRUFF = 0
c	I = ITRRNDATA(X,Y)
c	I = I .AND. MASKROUGH
c	JIPARAM = -LOCROUGH		!for F90 compilation
c	I = JISHFT(I,JIPARAM)
c	ITRRNRUFF = I

999	RETURN
	END


	INTEGER*4 FUNCTION ITRRNTREE ( X,Y)

	INTEGER*4	IVEG

	include 	'global.f'
	include 	'globtrrn.f'
	include		'globscr.f'


	A = TRRNVEGH (X,Y)
	ITRRNTREE = INT(A)

999	RETURN
	END



	SUBROUTINE TRRNLOC ( X, Y, IX, IY)

	include 	'global.f'
	include 	'globtrrn.f'

	REAL		X, Y
	INTEGER*4	IX,IY

	XA = X
	YA = Y

	IF ( X .GT. ZBX2 ) THEN
	  CALL X_ZONE (XA, YA)
	ELSEIF ( X .GT. ZBX1 ) THEN
	  YP = ZBM * XA + ZBB
	  IF ( YA .LT. YP ) THEN
	    CALL X_ZONE (XA, YA)
	  ENDIF
	ENDIF

	IX = MOD(NINT(XA*100.),10000)
	IY = MOD(NINT(YA*100.),10000)

	RETURN
	END

	SUBROUTINE TRRNLOC5 ( X, Y, IX, IY)

	include 	'global.f'
	include 	'globtrrn.f'

	REAL		X, Y
	INTEGER*4	IX,IY

	XA = X
	YA = Y

	IF ( X .GT. ZBX2 ) THEN
	  CALL X_ZONE (XA, YA)
	ELSEIF ( X .GT. ZBX1 ) THEN
	  YP = ZBM * XA + ZBB
	  IF ( YA .LT. YP ) THEN
	    CALL X_ZONE (XA, YA)
	  ENDIF
	ENDIF

	IX = MOD(NINT(XA*1000.),100000)
	IY = MOD(NINT(YA*1000.),100000)

	RETURN
	END

	SUBROUTINE TRRNLOC6 ( X, Y, IX, IY)

	include 	'global.f'
	include 	'globtrrn.f'

	REAL		X, Y
	INTEGER*4	IX,IY

	XA = X

	YA = Y

	CALL TRRN_X_OFFSET ( XOFF)
	CALL TRRN_Y_OFFSET ( YOFF)

	IF ( XA .LT. XOFF ) XA = XA + XOFF
	IF ( YA .LT. YOFF ) YA = YA + YOFF

	IF ( X .GT. ZBX2 ) THEN
	  CALL X_ZONE (XA, YA)
	ELSEIF ( X .GT. ZBX1 ) THEN
	  YP = ZBM * XA + ZBB
	  IF ( YA .LT. YP ) THEN
	    CALL X_ZONE (XA, YA)
	  ENDIF
	ENDIF

	IX = MOD(NINT(XA*100.),1000000)
	IY = MOD(NINT(YA*100.),1000000)

	RETURN
	END

	SUBROUTINE TRRNLOC_10_FIGURE_ALPHA ( X, Y, IX, IY)

	include 	'global.f'
	include 	'globtrrn.f'

	REAL		X, Y
	INTEGER*4	IX,IY

	XA = X

	YA = Y

	CALL TRRN_X_OFFSET ( XOFF)
	CALL TRRN_Y_OFFSET ( YOFF)

	IF ( XA .LT. XOFF ) XA = XA + XOFF
	IF ( YA .LT. YOFF ) YA = YA + YOFF

	IF ( X .GT. ZBX2 ) THEN
	  CALL X_ZONE (XA, YA)
	ELSEIF ( X .GT. ZBX1 ) THEN
	  YP = ZBM * XA + ZBB
	  IF ( YA .LT. YP ) THEN
	    CALL X_ZONE (XA, YA)
	  ENDIF
	ENDIF

	IX = MOD(NINT(XA*1000.),10000000)
	IY = MOD(NINT(YA*1000.),10000000)

	RETURN
	END

	SUBROUTINE TRRN_Y_OFFSET ( Y)

	include 	'global.f'
	include 	'globtrrn.f'

	Y = YREDUCE

	RETURN
	END

	SUBROUTINE TRRN_X_OFFSET ( X)

	include 	'global.f'
	include 	'globtrrn.f'

	X = XREDUCE

	RETURN
	END

	SUBROUTINE X_ZONE (X, Y)

	include 	'global.f'
	include 	'globtrrn.f'

	XA = 500000 - EZ - (( 500000 - X - EZ ) * COS2Y ) -
     *		(( Y - NZ ) * SIN2Y)
	YA = NZ + (( Y - NZ ) * COS2Y ) + (( 500000 - X - EZ ) * SIN2Y ) 

	X = XA
	Y = YA

	RETURN
	END

	SUBROUTINE TRRNCUT ( X, Y )

C	This routine checks that x,y is within map boundary. If not then x,y is
C	limited to map edge.

	include 	'global.f'
	include 	'globtrrn.f'

	REAL		X, Y

	IF (X .LT. XLL ) 	X = XLL
	IF (X .GT. XLL + XWIDE) X = XLL + XWIDE
	IF (Y .LT. YLL ) 	Y = YLL
	IF (Y .GT. YLL + XWIDE) Y = YLL + XWIDE

	RETURN
	END

	INTEGER*4 FUNCTION ITRRNMOV (X, Y, MTYPE)

	include		'glbparam.f'
	include		'globtrrn.f'

	IVEG = ITRRNSURF(X,Y)

	I = 100
	IF ( IVEG .EQ. 20 ) THEN
	  I = 100
	ELSEIF ( IVEG .GT. 60 ) THEN
	  I = KRIVERDLAY(MTYPE, IVEG-60)
	ELSEIF ( IVEG .GT. 40 ) THEN
	  I = KAREADGRAD(MTYPE, IVEG-40)
	ELSEIF (IVEG .GT. 20 ) THEN
	  I = KTREEDGRAD(MTYPE,IVEG-20)
	ELSEIF ( IVEG .GT. 0 ) THEN
	  I = KCITYDGRAD(MTYPE,IVEG)
	ENDIF

	ITRRNMOV = I

	RETURN
	END



	SUBROUTINE GET_SYMSCALE (IVIEW, SCALE)

	include		'global.f'
	include		'globzoom.f'

	IF ( DOSHRINK_ICONS .NE. 1 ) THEN
	  SCALE = 1
	ELSE
	  IF ( IVIEW .LE. 0 ) THEN
	    SCALE = 1
	  ELSE
	    SCALE = SCALEFAC(IVIEW)
	    IF (SCALE .LE. 0) SCALE = 1
c	    IF (SCALE .GT. 64) SCALE = 64
	    SCALE = 1/SCALE
	  ENDIF
	ENDIF

c	IF ( DEBUGMENU ) THEN
c	  PRINT *,'Icon scale is ',SCALE
c	ENDIF

	RETURN
	END

	SUBROUTINE GET_VIEWSCALE (IVIEW, SCALE)

	include		'global.f'
	include		'globscr.f'

	SCALE = FLOAT( KSCRNFILE(1,IVIEW) )

	RETURN
	END

	SUBROUTINE GET_SCALE (IVIEW, SCALE)

	include		'global.f'
	include		'globzoom.f'

	SCALE = SCALEFAC(IVIEW)

	IF ( DEBUGMENU ) THEN
	  PRINT *,'Zoom scale is ',SCALE
	ENDIF

	RETURN
	END

	SUBROUTINE LOS_CITY ( X,Y,Z,PLOS)

	include 	'global.f'
	include 	'globtrrn.f'

	PLOS = 1.0

	CALL TRRNCELL ( X,Y, IGRIDWORD )

	ELEV = TRRNELEV ( X,Y)

C --------- Do we need to consider CITIES?

	IF( (IGRIDWORD .AND. MASKCITY) .NE. 0 )  THEN
	  ITYP  =  INAREA( X, Y, 6 )
	  IF( ITYP .GT. 0.0 )  THEN
	    HEIGHT     =  KCITYHEI(ITYP)
	    IF ( (Z*1000) .LT. (HEIGHT+ ELEV) ) THEN
	      PLOS       =  PLOS * CITYPLOS(ITYP)!  ** EXPON
	    ENDIF
	  ENDIF
	ENDIF

	RETURN
	END

	SUBROUTINE LOS_TREE ( X,Y,Z,PLOS)

	include 	'global.f'
	include 	'globtrrn.f'

	PLOS = 1.0

	CALL TRRNCELL ( X,Y, IGRIDWORD )

	ELEV = TRRNELEV ( X,Y)

	IF( (IGRIDWORD .AND. MASKTREE) .NE. 0 )  THEN
	  ITYP  =  INAREA( X, Y, 5 )
	  IF( ITYP .GT. 0.0 )  THEN
	    HEIGHT     =  KTREEHEI(ITYP)
	    IF ( (Z*1000) .LT. (HEIGHT+ ELEV) ) THEN
	      PLOS       =  PLOS * TREEPLOS(ITYP)!  ** EXPON
	    ENDIF
	  ENDIF
	ENDIF

	RETURN
	END

	SUBROUTINE LOS_AREA ( X,Y,Z,PLOS)

	include 	'global.f'
	include 	'globtrrn.f'

	PLOS = 1.0

	CALL TRRNCELL ( X,Y, IGRIDWORD )

	ELEV = TRRNELEV ( X,Y)

	IF( (IGRIDWORD .AND. MASKAREA) .NE. 0 )  THEN
	  ITYP  =  INAREA( X, Y, 8 )
	  IF( ITYP .GT. 0.0 )  THEN
	    HEIGHT     =  KAREAHEI(ITYP)
	    IF ( (Z*1000) .LT. (HEIGHT+ ELEV) ) THEN
	      PLOS       =  PLOS * AREAPLOS(ITYP)!  ** EXPON
	    ENDIF
	  ENDIF
	ENDIF

	RETURN
	END


	SUBROUTINE TRRNELEV2_old ( X,Y, ALT )

	include 	'global.f'
	include 	'globtrrn.f'	
	include		'globrpt.f'

	! alt is returned in metres

	! first we work out the x and y cell coords

	XCELL  =  1.0 + ( (X-XLL) / GRIDSIZE )
	YCELL  =  1.0 + ( (Y-YLL) / GRIDSIZE )

	IXG  =  XCELL
	IYG  =  YCELL

	! check that it is on the map
	IF (IXG .LE. 0 .OR. IXG .GT. KDIMX .OR.
     *		IYG .LE. 0 .OR. IYG .GT. KDIMX ) THEN
	  ALT = 0
	  GOTO 999
	ENDIF

	! now work out the index into the mapxy array
	ICELL   =  IXG  +  (IYG-1) * KDIMX

	! calculate the elevation for that cell
	IELEV = MAPXY(ICELL)
	IELEV = IELEV .AND. MASKELEV
	ALT =  FLOAT ( IELEV ) * 0.2

	! now get the coords for the centre of the cell
	XCELL = XLL + (IXG * GRIDSIZE)
	YCELL = YLL + (IYG * GRIDSIZE)

	! find the distance in each dimension for the point from the centre
	DX = X - XCELL
	DY = Y - YCELL

	IF ( DX .GE. 0 ) THEN ! we need to go east
	  IF ( X .LT. XLL + XWIDE - GRIDSIZE) THEN
	    ICELL2   =  ICELL + 1
	  ELSE
	    ICELL2 = ICELL
	  ENDIF
	ELSE ! we need to go west
	  IF ( X .GT. XLL + GRIDSIZE) THEN
	    ICELL2   =  ICELL - 1
	  ELSE
	    ICELL2 = ICELL
	  ENDIF
	ENDIF

	! now use this to find which is the nearest neighbour cell
	IF ( ABS(DX) .GT. ABS(DY) ) THEN
	  IY2 = IYG
	  DIST = DX
	  IF ( DX .GT. 0 ) THEN
	    IX2 = IXG + 1
	  ELSE
	    IX2 = IXG - 1
	  ENDIF
	ELSE
	  DIST = DY
	  IX2 = IXG
	  IF ( DY .GT. 0 ) THEN
	    IY2 = IYG + 1
	  ELSE
	    IY2 = IYG - 1
	  ENDIF
	ENDIF

	! check the neighbour is on the map
	! if not, then use the elev for this cell
	IF (IX2 .LE. 0 .OR. IX2 .GT. KDIMX .OR.
     *		IY2 .LE. 0 .OR. IY2 .GT. KDIMX ) THEN
	  GOTO 999
	ENDIF

	ICELL   =  IX2  +  (IY2-1) * KDIMX
	IELEV = MAPXY(ICELL)
	IELEV = IELEV .AND. MASKELEV
	ALT2 =  FLOAT ( IELEV ) * 0.2

	DZ = ALT2 - ALT

	DZ = DZ * (ABS(DIST)/ GRIDSIZE)

	ALT = ALT + DZ

999	CONTINUE
	RETURN
	END

	SUBROUTINE TRRNELEV2 ( X,Y, ALT )

	include 	'global.f'
	include 	'globtrrn.f'
	include		'globrpt.f'

	MAXCELL = KDIMX * KDIMX

	! alt is returned in metres

	! first we work out the x and y cell coords

	XCELL  =  1.0 + ( (X-XLL) / GRIDSIZE )
	YCELL  =  1.0 + ( (Y-YLL) / GRIDSIZE )

	IXG  =  XCELL
	IYG  =  YCELL

	! check that it is on the map
	IF (IXG .LE. 0 .OR. IXG .GT. KDIMX .OR.
     *		IYG .LE. 0 .OR. IYG .GT. KDIMX ) THEN
	  ALT = 0
	  GOTO 999
	ENDIF

	! now work out the index into the mapxy array
	ICELL   =  IXG  +  (IYG-1) * KDIMX

	! calculate the elevation for that cell (elev is LL point of cell)
	ELEVLL = FLOAT(MAPXY(ICELL) .AND. MASKELEV)

	ICELL   =  ICELL + 1
	IF ( ICELL .GT. MAXCELL) THEN
	  ELEVUL = ELEVLL
	ELSE
	  ELEVLR  =  FLOAT( MAPXY(ICELL) .AND. MASKELEV )
	ENDIF

	ICELL   =  ICELL  +  KDIMX
	IF ( ICELL .GT. MAXCELL) THEN
	  ELEVUL = ELEVLL
	ELSE
	  ELEVUR  =  FLOAT( MAPXY(ICELL) .AND. MASKELEV )
	ENDIF

	ICELL   =  ICELL - 1
	IF ( ICELL .LT. 0 .OR. ICELL .GT. MAXCELL) THEN
	  ELEVUL = ELEVLL
	ELSE
	  ELEVUL  =  FLOAT( MAPXY(ICELL) .AND. MASKELEV )
	ENDIF

C----- Interpolate amongst the elevations

	DX  =  XCELL - FLOAT( IXG )
	DY  =  YCELL - FLOAT( IYG )

	E1  =  ELEVLL  +  DX * (ELEVLR-ELEVLL)
	E2  =  ELEVUL  +  DX * (ELEVUR-ELEVUL)

	ALT  =  ( E1  +  DY * (E2-E1) ) * 0.20	! Convert to meters

999	CONTINUE
	RETURN
	END

	SUBROUTINE RIVER_TIME ( IRIVERTYPE, IMOVER, CROSSING_TIME)
	! returns (as crossing_time) the time in minutes for the specified
	! mover type to cross a river of type irivertype

	include 	'global.f'
	include 	'globtrrn.f'
	include		'globrpt.f'

	IF ( IMOVER .LE. 0 ) THEN
	  CROSSING_TIME = TNEVER
	ELSE
	  CROSSING_TIME = KRIVERDLAY(IMOVER,IRIVERTYPE)
	ENDIF

	RETURN
	END

	SUBROUTINE RIVER_WIDTH ( IRIVERTYPE, WIDTH)

	include 	'global.f'
	include 	'globtrrn.f'

	WIDTH = RIVERWIDE(IRIVERTYPE)

	RETURN
	END

	SUBROUTINE RIVER_DEPTH ( IRIVERTYPE, DEPTH)

	include 	'global.f'
	include 	'globtrrn.f'

	DEPTH = RIVERDEPTH(IRIVERTYPE)

	RETURN
	END




	SUBROUTINE RIVER_FORD_DELAY ( IRIVERTYPE, IMOVER, DELAY)

	! returns delay for fording a creak in seconds

	include 	'global.f'
	include 	'globtrrn.f'	

	DELAY = TNEVER

	IF ( IMOVER .LE. 0 ) THEN
	  RETURN
	ENDIF

	ICLASS     =  KRIVERCLAS(IRIVERTYPE)	! Primary=1, Secondary=2

	IF ( ICLASS .NE. 3 ) THEN ! not a lake etc
	  DELAY = KRIVERDGRAD(IMOVER,IRIVERTYPE)
	ENDIF

	RETURN
	END





	FUNCTION  INANY  ( X,Y, ITYPE )

C-----------------------------------------------------------------------C
C									C
C	PURPOSE:  To determine if the point (X,Y) lies within the	C
C		  boundaries of a polygonal Terrain Feature of type	C
C		  ITYPE.  If it does, the "Sub-Type" of the Terrain	C
C		  Feature is returned in INAREA;  otherwise INAREA	C
C		  is returned as zero.					C
C									C
C-----------------------------------------------------------------------C
C									C
C		    ITYPE:	5  =  VEGETATION AREA			C
C				6  =  URBAN AREA			C
C				8  =  GENERIC AREA			C
C									C
C-----------------------------------------------------------------------C
C                                                                       C
C                                                                       C
C       Modification Log                                                C
C                                                                       C
C                                                                       C
C       Port to PC                                    9/25/97 - EVH     C
C       Striped VMS logical from INCLUDE file name and made lower       C
C       case.                                                           C
C                                                                       C
C                                                                       C
C-----------------------------------------------------------------------C

	INCLUDE 	'glbparam.f'
	INCLUDE 	'globtrrn.f'

C ----- Initialize answer to "NO"

	INANY  =  0

	IDATA = ITRRNDATA ( X,Y)

	IF ( .NOT. (IDATA .AND. MASKTREE) ) THEN
	  IF ( .NOT. (IDATA .AND. MASKCITY)) THEN
	    IF ( .NOT. (IDATA .AND. MASKAREA)) THEN
	      RETURN
	    ENDIF
	  ENDIF
	ENDIF

C ----- Loop over all poly-terrain features

	DO 100  I = 1, KNUMFEATS

	    ITYPE = KTRNFTYPE(I)

	    IF ( ITYPE .EQ. 4 ) THEN ! rivers
	      IRIVTYP    =  KTRNFSUBT(I)
              ICLASS     =  KRIVERCLAS(IRIVTYP)	! Primary=1, Secondary=2
	      IF ( ICLASS .NE. 3 ) THEN ! not a lake etc
	        GOTO 100
	      ENDIF
	    ENDIF

C --------- Skip if point not within Feature's bounding box.

	    IF( X .LT. TRNFXMIN(I) )  GOTO 100
	    IF( X .GT. TRNFXMAX(I) )  GOTO 100
	    IF( Y .LT. TRNFYMIN(I) )  GOTO 100
	    IF( Y .GT. TRNFYMAX(I) )  GOTO 100

C --------- Fetch this Feature's parameters.

	    NODE1  =  KTRNFPNTR(I)			! First node
	    NUMNODES  =  KTRNFNODES(I)			! Number of nodes

C --------- Is the point inside the polygon.

	    CALL PINPOLY ( X,Y, TRNODESX(NODE1),
     *				TRNODESY(NODE1), NUMNODES, IANS )

	    IF( IANS .GT. 0 )  THEN			! YES, return
		INANY  =  KTRNFSUBT(I)			! feature's Sub-Type
		GOTO 999
	    ENDIF

  100	CONTINUE

C ----- RETURN to calling routine

  999	CONTINUE

	RETURN
	END




	SUBROUTINE  TRRN_WHICH_ROAD  ( X, Y, IROAD )

	INCLUDE 	'glbparam.f'
	INCLUDE 	'globtrrn.f'
	include		'globrpt.f'
	include		'globlos.f'

	CHARACTER*32 BINARY$

C ----- Initialize answer to "NO"

	IDATA = ITRRNDATA ( X,Y)

	IF ( (IDATA .AND. MASKROAD) .EQ. 0) THEN
	  IROAD = 0
	  GOTO 999
	ENDIF

	IF ( IROAD .GT. 0 ) THEN
	  CALL TRRN_TEST_ROAD  ( IROAD, X, Y, IANSWER )
	  IROAD = IANSWER
	ENDIF

	IF ( IROAD .GT. 0 ) THEN
	  GOTO 999
	ENDIF

C ----- Loop over road poly-terrain features only

        IBEG  =  KSTRTFEAT(3)
        IEND  =  KLASTFEAT(3)

        IF( IBEG .LE. 0 )  GOTO 999

	IROAD = 0
	DO  I = IBEG, IEND
	  CALL TRRN_TEST_ROAD  ( I, X, Y, IANSWER )
	  IF ( IANSWER .GT. 0 ) THEN
	    IROAD = IANSWER
	    GOTO 999
	  ENDIF
	ENDDO

  999	CONTINUE

	RETURN
	END


	SUBROUTINE TRRN_TEST_ROAD  ( IROAD, X, Y, IANSWER )

	INCLUDE 	'glbparam.f'
	INCLUDE 	'globtrrn.f'
	include		'globrpt.f'
	include		'globlos.f'

C ----- Initialize answer to "NO"

	IANSWER = 0

	IF ( IROAD .LE. 0 ) GOTO 999

	IF ( KTRNFTYPE(IROAD) .NE. 3 ) GOTO 999

C --------- Get road half width for bounding box

	IROADTYP   =  KTRNFSUBT(IROAD)			! Road's TYPE
	HALFWIDTH  =  ROADWIDE(IROADTYP)

C --------- Skip if point not within ROAD's bounding box.

	IF( X .LT. TRNFXMIN(IROAD)-HALFWIDTH  )  GOTO 999
	IF( X .GT. TRNFXMAX(IROAD)+HALFWIDTH  )  GOTO 999
	IF( Y .LT. TRNFYMIN(IROAD)-HALFWIDTH  )  GOTO 999
	IF( Y .GT. TRNFYMAX(IROAD)+HALFWIDTH  )  GOTO 999

C --------- Setup to process this ROAD feature further.

	HWSQRD     =  HALFWIDTH * HALFWIDTH		! Square of road's
							! half-width (km)

	NODE1  =  KTRNFPNTR(IROAD)			! First node
	LAST   =  NODE1 + KTRNFNODES(IROAD) - 1		! Last node

C --------- Fetch first node's coordinates

	X2  =  TRNODESX(NODE1)
	Y2  =  TRNODESY(NODE1)

C --------- Loop over the segments comprising this road STRING.

	DO 100  J = NODE1+1, LAST

C ------------- Set "previous" end-point

	  X1  =  X2
	  Y1  =  Y2

C ------------- Fetch "next" end-point of string segment

	  X2  =  TRNODESX(J)
	  Y2  =  TRNODESY(J)

C ------------- Skip if point not within segment's bounding box.

	  IF( X .LT. MIN( X1,X2 )-HALFWIDTH  )  GOTO 100
	  IF( X .GT. MAX( X1,X2 )+HALFWIDTH  )  GOTO 100
	  IF( Y .LT. MIN( Y1,Y2 )-HALFWIDTH  )  GOTO 100
	  IF( Y .GT. MAX( Y1,Y2 )+HALFWIDTH  )  GOTO 100

C ------------- Test distance from point to segment's centerline.

	  CALL SQDISPTL ( X,Y, X1,Y1, X2,Y2, DSQRD )

	  IF( DSQRD .LE. HWSQRD )  THEN           ! Point is within
	    IANSWER = IROAD
	    GOTO 999
	  ENDIF

  100	    CONTINUE

  200	CONTINUE

C ----- RETURN to calling routine

  999	CONTINUE

	RETURN
	END


	SUBROUTINE  TRRN_WHICH_RIVER  ( X, Y, IRIVER )

	INCLUDE 	'glbparam.f'
	INCLUDE 	'globtrrn.f'
	include		'globrpt.f'
	include		'globlos.f'

	CHARACTER*32 BINARY$

C ----- Initialize answer to "NO"

	IDATA = ITRRNDATA ( X,Y)

	IF ( (IDATA .AND. MASKRIVER) .EQ. 0) THEN
	  IRIVER = 0
	  GOTO 999
	ENDIF

	IF ( IRIVER .GT. 0 ) THEN
	  CALL TRRN_TEST_RIVER  ( IRIVER, X, Y, IANSWER )
	  IRIVER = IANSWER
	ENDIF

	IF ( IRIVER .GT. 0 ) THEN
	  GOTO 999
	ENDIF

C ----- Loop over road poly-terrain features only

        IBEG  =  KSTRTFEAT(4)
        IEND  =  KLASTFEAT(4)

        IF( IBEG .LE. 0 )  GOTO 999

	IROAD = 0
	DO  I = IBEG, IEND
	  CALL TRRN_TEST_RIVER  ( I, X, Y, IANSWER )
	  IF ( IANSWER .GT. 0 ) THEN
	    IRIVER = IANSWER
	    GOTO 999
	  ENDIF
	ENDDO

  999	CONTINUE

	RETURN
	END


	SUBROUTINE TRRN_TEST_RIVER  ( IRIVER, X, Y, IANSWER )

	INCLUDE 	'glbparam.f'
	INCLUDE 	'globtrrn.f'
	include		'globrpt.f'
	include		'globlos.f'

C ----- Initialize answer to "NO"

	IANSWER = 0

	IF ( IRIVER .LE. 0 ) GOTO 999

	IF ( KTRNFTYPE(IRIVER) .NE. 4 ) GOTO 999

C --------- Get road half width for bounding box

	IRIVERTYP   =  KTRNFSUBT(IRIVER)
        ICLASS     =  KRIVERCLAS(IRIVERTYP)
	IF ( ICLASS .EQ. 3 ) THEN
	  GOTO 999
	ENDIF

C --------- Get road half width for bounding box

	HALFWIDTH  =  RIVERWIDE(IRIVERTYP)

C --------- Skip if point not within ROAD's bounding box.

	IF( X .LT. TRNFXMIN(IRIVER)-HALFWIDTH  )  GOTO 999
	IF( X .GT. TRNFXMAX(IRIVER)+HALFWIDTH  )  GOTO 999
	IF( Y .LT. TRNFYMIN(IRIVER)-HALFWIDTH  )  GOTO 999
	IF( Y .GT. TRNFYMAX(IRIVER)+HALFWIDTH  )  GOTO 999

C --------- Setup to process this ROAD feature further.

	HWSQRD     =  HALFWIDTH * HALFWIDTH		! Square of road's
							! half-width (km)

	NODE1  =  KTRNFPNTR(IRIVER)			! First node
	LAST   =  NODE1 + KTRNFNODES(IRIVER) - 1		! Last node

C --------- Fetch first node's coordinates

	X2  =  TRNODESX(NODE1)
	Y2  =  TRNODESY(NODE1)

C --------- Loop over the segments comprising this road STRING.

	DO 100  J = NODE1+1, LAST

C ------------- Set "previous" end-point

	  X1  =  X2
	  Y1  =  Y2

C ------------- Fetch "next" end-point of string segment

	  X2  =  TRNODESX(J)
	  Y2  =  TRNODESY(J)

C ------------- Skip if point not within segment's bounding box.

	  IF( X .LT. MIN( X1,X2 )-HALFWIDTH  )  GOTO 100
	  IF( X .GT. MAX( X1,X2 )+HALFWIDTH  )  GOTO 100
	  IF( Y .LT. MIN( Y1,Y2 )-HALFWIDTH  )  GOTO 100
	  IF( Y .GT. MAX( Y1,Y2 )+HALFWIDTH  )  GOTO 100

C ------------- Test distance from point to segment's centerline.

	  CALL SQDISPTL ( X,Y, X1,Y1, X2,Y2, DSQRD )

	  IF( DSQRD .LE. HWSQRD )  THEN           ! Point is within
	    IANSWER = IRIVER
	    GOTO 999
	  ENDIF

  100	    CONTINUE

  200	CONTINUE

C ----- RETURN to calling routine

  999	CONTINUE

	RETURN
	END


	SUBROUTINE TRRN_WHICH_AREA  ( IXAREA, X,Y, ITYPE, IDEBUG )

	INCLUDE 	'glbparam.f'
	INCLUDE 	'globtrrn.f'
	include		'globrpt.f'
	include		'globlos.f'

	CHARACTER*32 BINARY$

c	DX = X - XLL
c	DY = Y - YLL
c	SX = XLL + XWIDE - X

c	IF ( ITYPE .EQ. FEAT_VEG ) THEN
c	  CALL TRRN_WHICH_POLY  ( IXAREA, X,Y, DX, SX, DY, ITYPE, MASKTREE,
c     *		KTREEXMIN, KTREEXMAX, KTREEYMIN, KTREEYMAX, KNUMTREES, IDEBUG )
c	  RETURN
c	ELSEIF ( ITYPE .EQ. FEAT_CITY ) THEN
c	  CALL TRRN_WHICH_POLY  ( IXAREA, X,Y, DX, SX, DY, ITYPE, MASKCITY,
c     *		KCITYXMIN, KCITYXMAX, KCITYYMIN, KCITYYMAX, KNUMCITIES, IDEBUG )
c	  RETURN
c	ELSEIF ( ITYPE .EQ. FEAT_AREA ) THEN
c	  CALL TRRN_WHICH_POLY  ( IXAREA, X,Y, DX, SX, DY, ITYPE, MASKAREA,
c     *		KAREAXMIN, KAREAXMAX, KAREAYMIN, KAREAYMAX, KNUMAREAS, IDEBUG )
c	  RETURN
c	ENDIF

C ----- Initialize answer to "NO"

	JAREA  =  0

D	IF ( IDEBUG .GT. 0 )THEN
D	  WRITE (KLINE$,"(28x,'Last poly was ',I6)") IXAREA
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IDATA = ITRRNDATA ( X,Y)

D	IF ( IDEBUG .GT. 0 )THEN
D	  CALL I4_TO_BIN ( IDATA, BINARY$)
D	  WRITE (KLINE$,"(28X,'Data      ',A32)") BINARY$
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF ( ITYPE .EQ. 4 ) THEN

D	  IF ( IDEBUG .GT. 0 )THEN
D	    CALL I4_TO_BIN ( MASKRIVER,  BINARY$)
D	    WRITE (KLINE$,"(28X,'MASKRIVER ',A32)") BINARY$
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IF ( (IDATA .AND. MASKRIVER) .EQ. 0) THEN

D	    IF ( IDEBUG .GT. 0 )THEN
D	      WRITE (KLINE$,"(28x,'No water in this cell')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    GOTO 999
	  ENDIF

	ELSEIF ( ITYPE .EQ. 5 ) THEN

D	  IF ( IDEBUG .GT. 0 )THEN
D	    CALL I4_TO_BIN ( MASKTREE,  BINARY$)
D	    WRITE (KLINE$,"(28X,'MASKTREE  ',A32)") BINARY$
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IF ( (IDATA .AND. MASKTREE) .EQ. 0) THEN

D	    IF ( IDEBUG .GT. 0 )THEN
D	      WRITE (KLINE$,"(28x,'No trees in this cell')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    GOTO 999
	  ENDIF

	ELSEIF ( ITYPE .EQ. 6 ) THEN

D	  IF ( IDEBUG .GT. 0 )THEN
D	    CALL I4_TO_BIN ( MASKCITY,  BINARY$)
D	    WRITE (KLINE$,"(28X,'MASKCITY  ',A32)") BINARY$
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IF ( (IDATA .AND. MASKCITY) .EQ. 0) THEN

D	    IF ( IDEBUG .GT. 0 )THEN
D	      WRITE (KLINE$,"(28x,'No cities in this cell')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    GOTO 999
	  ENDIF

	ELSEIF ( ITYPE .EQ. 8 ) THEN

D	  IF ( IDEBUG .GT. 0 )THEN
D	    CALL I4_TO_BIN ( MASKAREA,  BINARY$)
D	    WRITE (KLINE$,"(28X,'MASKAREA  ',A32)") BINARY$
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IF ( (IDATA .AND. MASKAREA) .EQ. 0) THEN

D	    IF ( IDEBUG .GT. 0 )THEN
D	      WRITE (KLINE$,"(28x,'No areas in this cell')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    GOTO 999
	  ENDIF
	ENDIF

	CALL TRRN_TEST_AREA  ( IXAREA, X, Y, ITYPE )
	IF ( IXAREA .GT. 0 ) THEN

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'Still in that poly')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  JAREA = IXAREA
	  GOTO 999
	ENDIF

C ----- Loop over all poly-terrain features of specified type only

	IBEG  =  KSTRTFEAT(ITYPE)
	IEND  =  KLASTFEAT(ITYPE)

        IF( IBEG .LE. 0 ) GOTO 999

	DO I = IBEG, IEND
	  ITEST = I
	  CALL TRRN_TEST_AREA  ( ITEST, X,Y, ITYPE )
	  IF ( ITEST .GT. 0 ) THEN
	    JAREA = ITEST

D	    IF ( IDEBUG .GT. 0 )THEN
D	      WRITE (KLINE$,"(28x,'Found polygon ',I6)") JAREA
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    GOTO 999
	  ENDIF
	ENDDO

C ----- RETURN to calling routine

  999	CONTINUE

	IXAREA = JAREA

	RETURN
	END


	SUBROUTINE  TRRN_BUILDING  ( X1, Y1, X2, Y2, IBUILD, ALOS, BLOS, CLOS, X3, Y3, IDEBUG )

	INCLUDE 	'glbparam.f'
	INCLUDE 	'globtrrn.f'
	include		'globrpt.f'

	CHARACTER*32 BINARY$

	IDATA1 = ITRRNDATA ( X1,Y1)
	IDATA2 = ITRRNDATA ( X2,Y2)

D	IF ( IDEBUG .GT. 0 )THEN
D	  CALL I4_TO_BIN ( IDATA1, BINARY$)
D	  WRITE (KLINE$,"(28X,'Data1     ',A32)") BINARY$
D	  CALL TRACEOUT (0,0,0)
D	  CALL I4_TO_BIN ( IDATA2, BINARY$)
D	  WRITE (KLINE$,"(28X,'Data2     ',A32)") BINARY$
D	  CALL TRACEOUT (0,0,0)
D	  CALL I4_TO_BIN ( MASKBILD,  BINARY$)
D	  WRITE (KLINE$,"(28X,'MASKBILD  ',A32)") BINARY$
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF ( ( (IDATA1 .AND. MASKBILD) .EQ. 0) .AND. ((IDATA2 .AND. MASKBILD) .EQ. 0)) THEN

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'No buildings nearby')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IBUILD = 0
	  GOTO 999
	ENDIF

C ----- Loop over all buildings.

	IBUILD = 0

	XMAX = MAX(X1,X2)
	XMIN = MIN(X1,X2)
	YMAX = MAX(Y1,Y2)
	YMIN = MIN(Y1,Y2)

D	IF ( IDEBUG .GT. 0 )THEN
D	  WRITE (KLINE$,"(24x,'Looking for buildings')")
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(24x,'max box ',F12.3,XX,F12.3)") XMAX, YMAX
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(24x,'min box ',F12.3,XX,F12.3)") XMIN, YMIN
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(24x,'ALOS, BLOS, CLOS ',F12.3,XX,F12.3,XX,F12.3)") ALOS, BLOS, CLOS
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	DO 300  I = 1, KNUMBILDS

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'Testing building ',I6)") I
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(28x,'max box ',F12.3,XX,F12.3)") BILDXMAX(I), BILDYMAX(I)
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(28x,'min box ',F12.3,XX,F12.3)") BILDXMIN(I), BILDYMIN(I)
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

C --------- If bounding boxes don't overlap, no intersect possible

	  IF( XMAX .LT. BILDXMIN(I) )  GOTO 300
	  IF( XMIN .GT. BILDXMAX(I) )  GOTO 300
	  IF( YMAX .LT. BILDYMIN(I) )  GOTO 300
	  IF( YMIN .GT. BILDYMAX(I) )  GOTO 300

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'Boxes overlap')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

C-------------------------------------------------------------------------------
C---------- Bounding boxes overlap.  As a filter, compute the distance from
C---------- the center of the Building's Bounding Box (BBB) to the LOS ray,
C---------- and compare it to 1/2 the diagonal of the BBB.
C-------------------------------------------------------------------------------

C --------- Calculate the center of the BBB

	  XC  =  ( BILDXMIN(I) + BILDXMAX(I) ) * 0.5
	  YC  =  ( BILDYMIN(I) + BILDYMAX(I) ) * 0.5

C --------- Calculate the square of 1/2 the BBB diagonal

	  DX    =  BILDXMAX(I) - BILDXMIN(I)
	  DY    =  BILDYMAX(I) - BILDYMIN(I)
	  DIAG  =  ( DX*DX + DY*DY ) * 0.25

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'Diagonal for building ',F15.6)") DIAG
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

C --------- Calculate the square of the distance from (XC,YC) to LOS ray

	  CALL SQDISPTL ( XC,YC, X1,Y1, X2,Y2, DIST )

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'Distance from centre of building ',F15.6)") DIST
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

C --------- If DIST greater than DIAG, no intersect possible

	  IF( DIST .GE. DIAG )  THEN

D	    IF ( IDEBUG .GT. 0 )THEN
D	      WRITE (KLINE$,"(28x,'Too far away')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    GOTO 300
	  ENDIF

C-------------------------------------------------------------------------------
C---------- Intersect is possible, we will have to test the individual
C---------- segments (walls) which make up the building's outline.
C-------------------------------------------------------------------------------

C --------- Fetch building's node parameters

	  NODE1     =  KBILDPNTR1(I)		! First node
	  NUMNODES  =  KBILDNODES(I)		! Number of nodes
	  LAST      =  NODE1 + NUMNODES - 1	! Last node
	  ZWALL     =  0.0

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'This building has ',I6,' nodes')") NUMNODES
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

C --------- Fetch last node of building, set Right/Left flag

	  X  =  TRNODESX(LAST)
	  Y  =  TRNODESY(LAST)
	  IF(  ( ALOS*X + BLOS*Y )  .GT.  CLOS  )  THEN
	    IFLAG  =  1
	  ELSE
	    IFLAG  =  2
	  ENDIF

C --------- Loop over all outside walls.

	  DO  200  J = NODE1, LAST

C ------------- Store previous node

	        XP  =  X
	        YP  =  Y

C ------------- Fetch the coordinates of this node.  The line segment from
C               (XP,YP) to (X,Y) is the wall being tested.

		X  =  TRNODESX(J)
		Y  =  TRNODESY(J)

C ------------- Test this node for same side as preceding node

		TEST  =  ALOS * X  +  BLOS * Y

		IF( IFLAG .EQ. 1 )  THEN
		    IF( TEST .GT. CLOS )  GOTO 200
		ELSE
		    IF( TEST .LT. CLOS )  GOTO 200
		ENDIF

C ------------- Node is on opposite side as preceding node, check if LOS
C               ground trace intersects wall from previous node to this node.

		IFLAG  =  3 - IFLAG

		CALL LOSINSECT (  X1,Y1, X2,Y2,
     *				  ALOS,BLOS,CLOS,
     *				  XP,YP,  X,Y,  IANS, XI,YI )

D	        IF ( IDEBUG .GT. 0 )THEN
D	          WRITE (KLINE$,"(32x,'Does it intersect? ',I6)") IANS
D	          CALL TRACEOUT (0,0,0)
D	        ENDIF

		IF( IANS .EQ. 0 )  THEN
	          GOTO 200
	        ENDIF

C ------------- LOS ground trace intersects this wall, check heights

	        IBUILD = I
	        X3 = XI
	        Y3 = YI
	        GOTO 999

  200	    CONTINUE		! END loop over Segments

  300	CONTINUE		! END loop over Buildings

C ----- RETURN to calling routine.

  999	CONTINUE

	RETURN
	END




	SUBROUTINE  TRRN_GET_AREA_LOS  ( X, Y, PLOS, HEIGHT, KTYPE)

	INCLUDE		'glbparam.f'
	INCLUDE		'globtrrn.f'
	include		'globrpt.f'
	include		'globlos.f'

	INTEGER*4	IROAD / 0/
	INTEGER*4	IWATER / 0/
	INTEGER*4	ITREE / 0/
	INTEGER*4	ICITY / 0/
	INTEGER*4	IAREA / 0/

D	IF ( DEBUGLOS .GT. 0 )THEN
D	  WRITE (KLINE$,"(24x,'Last road was ',I6)") IROAD
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	CALL TRRN_WHICH_ROAD  ( X, Y, IROAD )

	ISUBTYPE = 0
	KTYPE = 0

	IF (IROAD .GT. 0 ) THEN
	  PLOS = 1.0
	  HEIGHT = 0.0
	  KTYPE = KTRNFTYPE(IROAD)
	  GOTO 999
	ENDIF

	PLOS = 1.0
	HEIGHT = 0

D	IF ( DEBUGLOS .GT. 0 )THEN
D	  WRITE (KLINE$,"(24x,'Last water was ',I6)") IWATER
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	CALL TRRN_TEST_POLY  ( X,Y, IWATER )
	IF ( IWATER .GT. 0 ) THEN ! still inside previous lake
	  ISUBTYPE = KTRNFSUBT(IWATER)
	  PLOS = 1.0
	  HEIGHT = 0.0
	  KTYPE = KTRNFTYPE(IWATER)
	  GOTO 999
	ENDIF

D	IF ( DEBUGLOS .GT. 0 )THEN
D	  WRITE (KLINE$,"(24x,'Last tree was ',I6)") ITREE
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	CALL TRRN_TEST_POLY  ( X,Y, ITREE )
	IF ( ITREE .GT. 0 ) THEN
	  ISUBTYPE = KTRNFSUBT(ITREE)
	  PLOS = TREEPLOS_STEP(ISUBTYPE)
	  HEIGHT = FLOAT(KTREEHEI(ISUBTYPE))
	  KTYPE = KTRNFTYPE(ITREE)
	  GOTO 999
	ENDIF

D	IF ( DEBUGLOS .GT. 0 )THEN
D	  WRITE (KLINE$,"(24x,'Last city was ',I6)") ICITY
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	CALL TRRN_TEST_POLY  ( X,Y, ICITY )
	IF ( ICITY .GT. 0 ) THEN
	  KTYPE = KTRNFTYPE(ICITY)
	  ISUBTYPE = KTRNFSUBT(ICITY)
	  PLOS = CITYPLOS_STEP(ISUBTYPE)
	  HEIGHT = FLOAT(KCITYHEI(ISUBTYPE))
	  GOTO 999
	ENDIF

D	IF ( DEBUGLOS .GT. 0 )THEN
D	  WRITE (KLINE$,"(24x,'Last area was ',I6)") IAREA
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	CALL TRRN_TEST_POLY  ( X,Y, IAREA )
	IF ( IAREA .GT. 0 ) THEN
	  KTYPE = KTRNFTYPE(IAREA)
	  ISUBTYPE = KTRNFSUBT(IAREA)
	  PLOS = AREAPLOS_STEP(ISUBTYPE)
	  HEIGHT = FLOAT(KAREAHEI(ISUBTYPE))
	  GOTO 999
	ENDIF

	CALL TRRN_WHICH_POLY ( X,Y, IFEAT)

	IF ( IFEAT .GT. 0 ) THEN
	  KTYPE = KTRNFTYPE(IFEAT)
	  ISUBTYPE = KTRNFSUBT(IFEAT)
	  IF ( KTYPE .EQ. FEAT_AREA ) THEN
	    PLOS = AREAPLOS_STEP(ISUBTYPE)
	    HEIGHT = FLOAT(KAREAHEI(ISUBTYPE))
	    IAREA = IFEAT
	  ELSEIF ( KTYPE .EQ. FEAT_CITY ) THEN
	    PLOS = CITYPLOS_STEP(ISUBTYPE)
	    HEIGHT = FLOAT(KCITYHEI(ISUBTYPE))
	    ICITY = IFEAT
	  ELSEIF ( KTYPE .EQ. FEAT_RIVER ) THEN
	    PLOS = 1.0
	    HEIGHT = 0.0
	    IRIVER = IFEAT
	  ELSEIF ( KTYPE .EQ. FEAT_VEG ) THEN
	    PLOS = TREEPLOS_STEP(ISUBTYPE)
	    HEIGHT = FLOAT(KTREEHEI(ISUBTYPE))
	    ITREE = IFEAT
	  ENDIF
	ENDIF

999	CONTINUE

D	IF (DEBUGLOS .GT. 0) THEN
D	  WRITE (KLINE$, "(24x,'road ',I6)") IROAD
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$, "(24x,'water ',I6)") IWATER
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$, "(24x,'tree ',I6)") ITREE
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$, "(24x,'city ',I6)") ICITY
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$, "(24x,'area ',I6)") IAREA
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$, "(24x,'feature height = ',F12.3)") HEIGHT
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$, "(24x,'feature plos =   ',F12.3)") PLOS
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	RETURN
	END









	SUBROUTINE  TRRN_WHICH_BUILDING ( X1, Y1, X2, Y2, ALOS, BLOS, CLOS, IBUILD, IDEBUG )

	INCLUDE 	'glbparam.f'
	INCLUDE 	'globtrrn.f'
C	INCLUDE		'globlos.f'		! "Scratch" for LOS
	include		'globrpt.f'

	CHARACTER*32 BINARY$

	IDATA1 = ITRRNDATA ( X1,Y1)
	IDATA2 = ITRRNDATA ( X2,Y2)

D	IF ( IDEBUG .GT. 0 )THEN
D	  CALL I4_TO_BIN ( IDATA1, BINARY$)
D	  WRITE (KLINE$,"(28X,'Data1     ',A32)") BINARY$
D	  CALL TRACEOUT (0,0,0)
D	  CALL I4_TO_BIN ( IDATA2, BINARY$)
D	  WRITE (KLINE$,"(28X,'Data2     ',A32)") BINARY$
D	  CALL TRACEOUT (0,0,0)
D	  CALL I4_TO_BIN ( MASKBILD,  BINARY$)
D	  WRITE (KLINE$,"(28X,'MASKBILD  ',A32)") BINARY$
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF ( ( (IDATA1 .AND. MASKBILD) .EQ. 0) .AND. ((IDATA2 .AND. MASKBILD) .EQ. 0)) THEN

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'No buildings nearby')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IBUILD = 0
	  GOTO 999
	ENDIF

C ----- Loop over all buildings.

	IBUILD = 0

	XMAX = MAX(X1,X2)
	XMIN = MIN(X1,X2)
	YMAX = MAX(Y1,Y2)
	YMIN = MIN(Y1,Y2)

D	IF ( IDEBUG .GT. 0 )THEN
D	  WRITE (KLINE$,"(24x,'Looking for buildings')")
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(24x,'max box ',F12.3,XX,F12.3)") XMAX, YMAX
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(24x,'min box ',F12.3,XX,F12.3)") XMIN, YMIN
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	DO 300  I = 1, KNUMBILDS

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'Testing building ',I6)") I
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(28x,'max box ',F12.3,XX,F12.3)") BILDXMAX(I), BILDYMAX(I)
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(28x,'min box ',F12.3,XX,F12.3)") BILDXMIN(I), BILDYMIN(I)
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

C --------- If bounding boxes don't overlap, no intersect possible

	  IF( XMAX .LT. BILDXMIN(I) )  GOTO 300
	  IF( XMIN .GT. BILDXMAX(I) )  GOTO 300
	  IF( YMAX .LT. BILDYMIN(I) )  GOTO 300
	  IF( YMIN .GT. BILDYMAX(I) )  GOTO 300

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'Boxes overlap')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

C-------------------------------------------------------------------------------
C---------- Bounding boxes overlap.  As a filter, compute the distance from
C---------- the center of the Building's Bounding Box (BBB) to the LOS ray,
C---------- and compare it to 1/2 the diagonal of the BBB.
C-------------------------------------------------------------------------------

C --------- Calculate the center of the BBB

	  XC  =  ( BILDXMIN(I) + BILDXMAX(I) ) * 0.5
	  YC  =  ( BILDYMIN(I) + BILDYMAX(I) ) * 0.5

C --------- Calculate the square of 1/2 the BBB diagonal

	  DX    =  BILDXMAX(I) - BILDXMIN(I)
	  DY    =  BILDYMAX(I) - BILDYMIN(I)
	  DIAG  =  ( DX*DX + DY*DY ) * 0.25

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'Diagonal for building ',F15.6)") DIAG
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

C --------- Calculate the square of the distance from (XC,YC) to LOS ray

	  CALL SQDISPTL ( XC,YC, X1,Y1, X2,Y2, DIST )

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'Distance from centre of building ',F15.6)") DIST
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

C --------- If DIST greater than DIAG, no intersect possible

	  IF( DIST .GE. DIAG )  THEN

D	    IF ( IDEBUG .GT. 0 )THEN
D	      WRITE (KLINE$,"(28x,'Too far away')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    GOTO 300
	  ENDIF

C-------------------------------------------------------------------------------
C---------- Intersect is possible, we will have to test the individual
C---------- segments (walls) which make up the building's outline.
C-------------------------------------------------------------------------------

C --------- Fetch building's node parameters

	  NODE1     =  KBILDPNTR1(I)		! First node
	  NUMNODES  =  KBILDNODES(I)		! Number of nodes
	  LAST      =  NODE1 + NUMNODES - 1	! Last node
	  ZWALL     =  0.0

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'This building has ',I6,' nodes')") NUMNODES
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

C --------- Fetch last node of building, set Right/Left flag

	  X  =  TRNODESX(LAST)
	  Y  =  TRNODESY(LAST)
	  IF(  ( ALOS*X + BLOS*Y )  .GT.  CLOS  )  THEN
	    IFLAG  =  1
	  ELSE
	    IFLAG  =  2
	  ENDIF

C --------- Loop over all outside walls.

	  DO  200  J = NODE1, LAST

C ------------- Store previous node

	        XP  =  X
	        YP  =  Y

C ------------- Fetch the coordinates of this node.  The line segment from
C               (XP,YP) to (X,Y) is the wall being tested.

		X  =  TRNODESX(J)
		Y  =  TRNODESY(J)

C ------------- Test this node for same side as preceding node

		TEST  =  ALOS * X  +  BLOS * Y

		IF( IFLAG .EQ. 1 )  THEN
		    IF( TEST .GT. CLOS )  GOTO 200
		ELSE
		    IF( TEST .LT. CLOS )  GOTO 200
		ENDIF

C ------------- Node is on opposite side as preceding node, check if LOS
C               ground trace intersects wall from previous node to this node.

		IFLAG  =  3 - IFLAG

		CALL LOSINSECT (  X1,Y1, X2,Y2,
     *				  ALOS,BLOS,CLOS,
     *				  XP,YP,  X,Y,  IANS, XI,YI )

D	        IF ( IDEBUG .GT. 0 )THEN
D	          WRITE (KLINE$,"(32x,'Does it intersect? ',I6)") IANS
D	          CALL TRACEOUT (0,0,0)
D	        ENDIF

		IF( IANS .EQ. 0 )  THEN

D	          IF ( IDEBUG .GT. 0 )THEN
D	            WRITE (KLINE$,"(32x,'No')")
D	            CALL TRACEOUT (0,0,0)
D	          ENDIF

	          GOTO 200
	        ENDIF

D	        IF ( IDEBUG .GT. 0 )THEN
D	          WRITE (KLINE$,"(32x,'Yes')")
D	          CALL TRACEOUT (0,0,0)
D	        ENDIF

	        IBUILD = I
	        GOTO 999

  200	    CONTINUE		! END loop over Segments

  300	CONTINUE		! END loop over Buildings

C ----- RETURN to calling routine.

  999	CONTINUE

	RETURN
	END



	SUBROUTINE  TRRN_STRINGLOS  ( X1, Y1, Z1, X2, Y2, Z2, PLOS, HEIGHT, IDEBUG )

	INCLUDE 	'glbparam.f'
	INCLUDE 	'globtrrn.f'
	INCLUDE		'globlos.f'		! "Scratch" for LOS
	include		'globrpt.f'

	INTEGER*4	IBUILD / 0 /

	CHARACTER*32 BINARY$

D	IF ( IDEBUG .GT. 0 )THEN
D	  WRITE (KLINE$,"(24X,'Assesing strings')")
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	HEIGHT = 0.0

	IDATA1 = ITRRNDATA ( X1,Y1)
	IDATA2 = ITRRNDATA ( X2,Y2)

D	IF ( IDEBUG .GT. 0 )THEN
D	  CALL I4_TO_BIN ( IDATA1, BINARY$)
D	  WRITE (KLINE$,"(28X,'Data1     ',A32)") BINARY$
D	  CALL TRACEOUT (0,0,0)
D	  CALL I4_TO_BIN ( IDATA2, BINARY$)
D	  WRITE (KLINE$,"(28X,'Data2     ',A32)") BINARY$
D	  CALL TRACEOUT (0,0,0)
D	  CALL I4_TO_BIN ( MASKSTR,  BINARY$)
D	  WRITE (KLINE$,"(28X,'MASKSTR   ',A32)") BINARY$
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF ( ( (IDATA1 .AND. MASKSTR) .EQ. 0) .AND. ((IDATA2 .AND. MASKSTR) .EQ. 0)) THEN

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'No strings nearby')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IBUILD = 0
	  GOTO 999
	ENDIF

C ----- Loop over all

	IBUILD = 0

	XMAX = MAX(X1,X2)
	XMIN = MIN(X1,X2)
	YMAX = MAX(Y1,Y2)
	YMIN = MIN(Y1,Y2)
	ZMIN = MIN(Z1,Z2)

D	IF ( IDEBUG .GT. 0 )THEN
D	  WRITE (KLINE$,"(24x,'Looking for strings')")
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(24x,'max box ',F12.3,XX,F12.3)") XMAX, YMAX
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(24x,'min box ',F12.3,XX,F12.3)") XMIN, YMIN
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(24x,'min height ',F12.3)") ZMIN
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	DO 300  I = 1, KNUMFEATS
	  ITYPE = KTRNFTYPE(I)
	  IF ( ITYPE .NE. 7 ) GOTO 300

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'Testing feature ',I6)") I
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(28x,'max box ',F12.3,XX,F12.3)") TRNFXMAX(I), TRNFYMAX(I)
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(28x,'min box ',F12.3,XX,F12.3)") TRNFXMIN(I), TRNFYMIN(I)
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

C --------- If bounding boxes don't overlap, no intersect possible

	  IF( XMAX .LT. TRNFXMIN(I) )  GOTO 300
	  IF( XMIN .GT. TRNFXMAX(I) )  GOTO 300
	  IF( YMAX .LT. TRNFYMIN(I) )  GOTO 300
	  IF( YMIN .GT. TRNFYMAX(I) )  GOTO 300

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'Boxes overlap')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  ISUBTYPE  =  KTRNFSUBT(I)
	  HB   =  KSTRHEI(ISUBTYPE)

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(32x,'String height ',F12.3)") HB
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IF ( HB .LT. ZMIN ) THEN
	    GOTO 300
	  ENDIF
C-------------------------------------------------------------------------------
C---------- Intersect is possible, we will have to test the individual
C---------- segments (walls) which make up the building's outline.
C-------------------------------------------------------------------------------

C --------- Fetch building's node parameters

	  NODE1     =  KTRNFPNTR(I)		! First node
	  NUMNODES  =  KTRNFNODES(I)		! Number of nodes
	  LAST      =  NODE1 + NUMNODES - 1	! Last node
	  ZWALL     =  0.0

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'This string has ',I6,' nodes')") NUMNODES
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

C --------- Fetch last node of building, set Right/Left flag

	  X  =  TRNODESX(NODE1)
	  Y  =  TRNODESY(NODE1)
	  IF(  ( ALOS*X + BLOS*Y )  .GT.  CLOS  )  THEN
	    IFLAG  =  1
	  ELSE
	    IFLAG  =  2
	  ENDIF

C --------- Loop over all outside walls.

	  DO  200  J = NODE1+1, LAST

C ------------- Store previous node

	        XP  =  X
	        YP  =  Y

C ------------- Fetch the coordinates of this node.  The line segment from
C               (XP,YP) to (X,Y) is the wall being tested.

		X  =  TRNODESX(J)
		Y  =  TRNODESY(J)

C ------------- Test this node for same side as preceding node

		TEST  =  ALOS * X  +  BLOS * Y

		IF( IFLAG .EQ. 1 )  THEN
		    IF( TEST .GT. CLOS )  GOTO 200
		ELSE
		    IF( TEST .LT. CLOS )  GOTO 200
		ENDIF

C ------------- Node is on opposite side as preceding node, check if LOS
C               ground trace intersects wall from previous node to this node.

		IFLAG  =  3 - IFLAG

		CALL LOSINSECT (  X1,Y1, X2,Y2,
     *				  ALOS,BLOS,CLOS,
     *				  XP,YP,  X,Y,  IANS, XI,YI )

D	        IF ( IDEBUG .GT. 0 )THEN
D	          WRITE (KLINE$,"(32x,'Does it intersect? ',I6)") IANS
D	          CALL TRACEOUT (0,0,0)
D	        ENDIF

		IF( IANS .EQ. 0 )  THEN

D	          IF ( IDEBUG .GT. 0 )THEN
D	            WRITE (KLINE$,"(32x,'No')")
D	            CALL TRACEOUT (0,0,0)
D	          ENDIF

	          GOTO 200
	        ENDIF

D	        IF ( IDEBUG .GT. 0 )THEN
D	          WRITE (KLINE$,"(32x,'Yes')")
D	          CALL TRACEOUT (0,0,0)
D	        ENDIF

	        PLOS = 0.0
	        HEIGHT = HB
	        GOTO 999

  200	    CONTINUE		! END loop over Segments

  300	CONTINUE		! END loop over Buildings

C ----- RETURN to calling routine.

  999	CONTINUE

	RETURN
	END




	 */

	/*
	 * Test functions
	 */
	public void makeTestMap(){
		lowerLeft = new Coordinate(100.0,100.0);
		upperRight = new Coordinate(110.0,110.0);
		
		this.addAreaType(new AreaType("Light_veg"));
		this.addAreaType(new AreaType("Medium_veg"));
		this.addAreaType(new AreaType("Dense_veg"));
		this.addAreaType(new AreaType("Lake"));
		this.addRiverType(new RiverType("River"));
		this.addRiverType(new RiverType("Creek"));
		this.addRoadType(new RoadType("Highway"));
		this.addRoadType(new RoadType("Road"));
		this.addRoadType(new RoadType("Track"));
		this.addBuildingType(new BuildingType("Brick_Building"));
		this.addBuildingType(new BuildingType("Wood_Building"));
		this.addWallType(new WallType("Wall"));
		this.addWallType(new WallType("Fence"));
		this.addWallType(new WallType("Hedge"));

		//FIXME - don't use new color but just 3 values - as this validates it properly
		this.areaTypeList.getFeatureType("Light_veg").setColor(new Color(145,240,70));
		this.areaTypeList.getFeatureType("Medium_veg").setColor(new Color(70,180,70));
		this.areaTypeList.getFeatureType("Dense_veg").setColor(new Color(35,95,35));
		this.areaTypeList.getFeatureType("Lake").setColor(new Color(10,150,170));
		this.riverTypeList.getFeatureType("River").setColor(new Color(10,150,170));
		this.riverTypeList.getFeatureType("Creek").setColor(new Color(45,170,120));
		this.roadTypeList.getFeatureType("Highway").setColor(new Color(190,55,40));
		this.roadTypeList.getFeatureType("Road").setColor(new Color(250,250,30));
		this.roadTypeList.getFeatureType("Track").setColor(new Color(180,10,35));
		this.buildingTypeList.getFeatureType("Brick_Building").setColor(new Color(240,135,130));
		this.buildingTypeList.getFeatureType("Wood_Building").setColor(new Color(10,10,10));
		this.wallTypeList.getFeatureType("Wall").setColor(new Color(10,10,10));
		this.wallTypeList.getFeatureType("Fence").setColor(new Color(90,90,90));
		this.wallTypeList.getFeatureType("Hedge").setColor(new Color(15,180,60));
		
		Area area = new Area(this.getAreaType("Light_veg"));
		area.getCoordinateList().addCoordinate(new Coordinate(101.0,101.0));
		area.getCoordinateList().addCoordinate(new Coordinate(102.0,101.0));
		area.getCoordinateList().addCoordinate(new Coordinate(102.0,102.0));
		area.getCoordinateList().addCoordinate(new Coordinate(101.0,102.0));
		this.addArea(area);

		area = new Area(this.getAreaType("Medium_veg"));
		area.getCoordinateList().addCoordinate(new Coordinate(102.0,102.0));
		area.getCoordinateList().addCoordinate(new Coordinate(103.0,102.0));
		area.getCoordinateList().addCoordinate(new Coordinate(103.0,103.0));
		area.getCoordinateList().addCoordinate(new Coordinate(102.0,103.0));
		this.addArea(area);
		
		area = new Area(this.getAreaType("Dense_veg"));
		area.getCoordinateList().addCoordinate(new Coordinate(103.0,103.0));
		area.getCoordinateList().addCoordinate(new Coordinate(104.0,103.0));
		area.getCoordinateList().addCoordinate(new Coordinate(104.0,104.0));
		area.getCoordinateList().addCoordinate(new Coordinate(103.0,104.0));
		this.addArea(area);
		
		area = new Area(this.getAreaType("Lake"));
		area.getCoordinateList().addCoordinate(new Coordinate(104.0,104.0));
		area.getCoordinateList().addCoordinate(new Coordinate(105.0,104.0));
		area.getCoordinateList().addCoordinate(new Coordinate(105.0,105.0));
		area.getCoordinateList().addCoordinate(new Coordinate(104.0,105.0));
		this.addArea(area);
		
		Building building = new Building(this.getBuildingType("Brick_Building"));
		building.getCoordinateList().addCoordinate(new Coordinate(105.0,105.0));
		building.getCoordinateList().addCoordinate(new Coordinate(106.0,105.0));
		building.getCoordinateList().addCoordinate(new Coordinate(106.0,106.0));
		building.getCoordinateList().addCoordinate(new Coordinate(105.0,106.0));
		this.addBuilding(building);

		Road road = new Road(this.getRoadType("Highway"));
		road.getCoordinateList().addCoordinate(new Coordinate(106.0,106.0));
		road.getCoordinateList().addCoordinate(new Coordinate(107.0,106.0));
		road.getCoordinateList().addCoordinate(new Coordinate(107.0,107.0));
		road.getCoordinateList().addCoordinate(new Coordinate(106.0,107.0));
		this.addRoad(road);

		River river = new River(this.getRiverType("River"));
		river.getCoordinateList().addCoordinate(new Coordinate(107.0,107.0));
		river.getCoordinateList().addCoordinate(new Coordinate(108.0,107.0));
		river.getCoordinateList().addCoordinate(new Coordinate(108.0,108.0));
		river.getCoordinateList().addCoordinate(new Coordinate(107.0,108.0));
		this.addRiver(river);
	}
	
	public void trace(){
		Tracer.write("report for map " + this.getName());
		Tracer.write("lower left " + lowerLeft.toString());
		Tracer.write("upper right " + upperRight.toString());
		Tracer.write("width " + this.getWidth()+"km");
		Tracer.write("height " + this.getHeight()+"km");
		Tracer.write("area types: " + this.areaTypeList.getSize());
		for (String s: this.areaTypeList.keySet()){
			AreaType areaType = (AreaType) this.areaTypeList.getFeatureType(s);
			Iterator<Feature> iterator = this.getAreaIterator();
			int count = 0;
			while (iterator.hasNext()){
				count++;
				Area area = (Area) iterator.next();
				if (area.getFeatureType() == areaType){
					count++;
				}
			}
			Tracer.write("    " + areaType.getName() + " : " + count
					+ "\n        color : " + areaType.getColor()
					+ "\n        height: " + areaType.getHeightM()
					+ "\n        water : " + areaType.getWater()
					+ "\n        PLOS  : " + areaType.getPLOS()
					);
		}
		Tracer.write("road types: " + this.roadTypeList.getSize());
		for (String s: this.roadTypeList.keySet()){
			RoadType roadType = (RoadType) this.roadTypeList.getFeatureType(s);
			Iterator<Feature> iterator = this.getRoadIterator();
			int count = 0;
			while (iterator.hasNext()){
				count++;
				Road road = (Road) iterator.next();
				if (road.getFeatureType() == roadType){
					count++;
				}
			}
			Tracer.write("    " + roadType.getName() + " : " + count +
					" color " + roadType.getColor());
		}
		Tracer.write("river types: " + this.riverTypeList.getSize());
		for (String s: this.riverTypeList.keySet()){
			RiverType riverType = (RiverType) this.riverTypeList.getFeatureType(s);
			Iterator<Feature> iterator = this.getRiverIterator();
			int count = 0;
			while (iterator.hasNext()){
				count++;
				River river = (River) iterator.next();
				if (river.getFeatureType() == riverType){
					count++;
				}
			}
			Tracer.write("    " + riverType.getName() + " : " + count +
					" color " + riverType.getColor());
		}
		Tracer.write("wall types: " + this.wallTypeList.getSize());
		for (String s: this.wallTypeList.keySet()){
			WallType wallType = (WallType) this.wallTypeList.getFeatureType(s);
			Iterator<Feature> iterator = this.getWallIterator();
			int count = 0;
			while (iterator.hasNext()){
				count++;
				Wall wall = (Wall) iterator.next();
				if (wall.getFeatureType() == wallType){
					count++;
				}
			}
			Tracer.write("    " + wallType.getName() + " : " + count +
					" color " + wallType.getColor());
		}
		Tracer.write("building types: " + this.buildingTypeList.getSize());
		for (String s: this.buildingTypeList.keySet()){
			BuildingType buildingType = (BuildingType) this.buildingTypeList.getFeatureType(s);
			Iterator<Feature> iterator = this.getBuildingIterator();
			int count = 0;
			while (iterator.hasNext()){
				count++;
				Building building = (Building) iterator.next();
				if (building.getFeatureType() == buildingType){
					count++;
				}
			}
			Tracer.write("    " + buildingType.getName() + " : " + count +
					" color " + buildingType.getColor());
		}

		/*
		Tracer.write("areas: " + this.areaList.size());
		Iterator<Area> iterator = this.getAreaIterator();
		int count = 0;
		while (iterator.hasNext()){
			count++;
			Area area = iterator.next();
			Tracer.write("area " + count + " type: " + area.getType().getName());
			for (Coordinate c: area.getCoordinates()){
				Tracer.write("    " + c);
			}
		}
		
		Tracer.write("buildings: " + this.buildingList.size());
		Iterator<Building> buildingIterator = this.getBuildingIterator();
		count = 0;
		while (buildingIterator.hasNext()){
			count++;
			Building building = buildingIterator.next();
			Tracer.write("building " + count + " type: " + building.getType().getName());
			for (Coordinate c: building.getCoordinates()){
				Tracer.write("    " + c);
			}
		}

		Tracer.write("roads: " + this.roadList.size());
		Iterator<Road> roadIterator = this.getRoadIterator();
		count = 0;
		while (roadIterator.hasNext()){
			count++;
			Road road = roadIterator.next();
			Tracer.write("road " + count + " type: " + road.getType().getName());
			for (Coordinate c: road.getCoordinates()){
				Tracer.write("    " + c);
			}
		}

		Tracer.write("rivers: " + this.riverList.size());
		Iterator<River> riverIterator = this.getRiverIterator();
		count = 0;
		while (riverIterator.hasNext()){
			count++;
			River river = riverIterator.next();
			Tracer.write("river " + count + " type: " + river.getType().getName());
			for (Coordinate c: river.getCoordinates()){
				Tracer.write("    " + c);
			}
		}
		*/
		Tracer.write("elevation model " + elevationModel.getWidth() +
				" cells wide and " + elevationModel.getHeight() + " cells high");
		Tracer.write("highest point " + elevationModel.getHighestM()+"m");
		Tracer.write(" lowest point " + elevationModel.getLowestM()+"m");
	}
}
	
