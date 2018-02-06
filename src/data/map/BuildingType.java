package data.map;

public class BuildingType extends FeatureType{

	public BuildingType (String name){
		super.setName(name);
	}
	
	/*
	 * from trrnlib
	SUBROUTINE TRRN_GET_BUILD_GRAD ( IBUILD, BGRAD)

	INCLUDE 	'glbparam.f'
	INCLUDE 	'globtrrn.f'
	include		'globrpt.f'

	REAL*4		PLOS

	ITYPE = 0
	BGRAD = 0.0

	IF (IBUILD .LE. 0 ) RETURN

	ITYPE = KBILDTYPE(IBUILD)
	BGRAD = BILDGRADFACT(ITYPE)

	RETURN
	END

C	REAL	   BILDFORTTIM(4,NUMBILDTYPES)	! Engineer minutes to reach fortification levels 1-4.
C	REAL       BILDGRADFACT(NUMBILDTYPES)   ! Building Rubbling Factor
C	INTEGER*2  KBILDROOMS  (NUMBILDTYPES)	! Total # of rooms.
C	BYTE	   KBILDCONTYP (NUMBILDTYPES)	! Construction type.
C	BYTE	   KBILDCLAS   (NUMBILDTYPES)	! Functional classification.
	 * 
	 * 
	SUBROUTINE TRRN_GET_FLOOR_HEIGHT ( IBUILD, F_HEIGHT)

	INCLUDE 	'glbparam.f'
	INCLUDE 	'globtrrn.f'
	include		'globrpt.f'

	ITYPE = 0
	HEIGHT = 0.0
	IF (IBUILD .LE. 0 ) RETURN

	ITYPE = KBILDTYPE(IBUILD)
	HEIGHT = BILDHEIGHT(ITYPE)
	IFLOORS = KBILDFLOORS(ITYPE)

	F_HEIGHT = HEIGHT / IFLOORS

	RETURN
	END


	SUBROUTINE TRRN_GET_BUILD_FLOORS ( IBUILD, IFLOORS)

	INCLUDE 	'glbparam.f'
	INCLUDE 	'globtrrn.f'
	include		'globrpt.f'

	ITYPE = 0
	IFLOORS = 0
	IF (IBUILD .LE. 0 ) RETURN

	ITYPE = KBILDTYPE(IBUILD)
	IFLOORS = KNUMFLOORS(IBUILD)

c	IFLOORS = KBILDFLOORS(ITYPE)

	RETURN
	END

	SUBROUTINE TRRN_GET_BUILD_HEIGHT ( IBUILD, HEIGHT)

	INCLUDE 	'glbparam.f'
	INCLUDE 	'globtrrn.f'
	include		'globrpt.f'

	ITYPE = 0
	HEIGHT = 0.0
	IF (IBUILD .LE. 0 ) RETURN

	ITYPE = KBILDTYPE(IBUILD)
	HEIGHT = BUILDING_HEIGHT(IBUILD)

C	HEIGHT = BILDHEIGHT(ITYPE)

	RETURN
	END
	
		SUBROUTINE  TRRN_WHICH_WALL ( X1, Y1, X2, Y2, ALOS, BLOS, CLOS, IBUILD, IWALL, IDEBUG )

	INCLUDE 	'glbparam.f'
	INCLUDE 	'globtrrn.f'
C	INCLUDE		'globlos.f'		! "Scratch" for LOS
	include		'globrpt.f'

	CHARACTER*32 BINARY$

	IDATA1 = ITRRNDATA ( X1,Y1)
	IDATA2 = ITRRNDATA ( X2,Y2)

	IWALL = 0
	ANGLE = 0.0

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

	  GOTO 999
	ENDIF

	XMAX = MAX(X1,X2)
	XMIN = MIN(X1,X2)
	YMAX = MAX(Y1,Y2)
	YMIN = MIN(Y1,Y2)

	I = IBUILD

D	IF ( IDEBUG .GT. 0 )THEN
D	  WRITE (KLINE$,"(24x,'Looking for buildings')")
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(24x,'max box ',F12.3,XX,F12.3)") XMAX, YMAX
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(24x,'min box ',F12.3,XX,F12.3)") XMIN, YMIN
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(24x,'ALOS, BLOS, CLOS ',F12.3,XX,F12.3,XX,F12.3)") ALOS, BLOS, CLOS
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(24x,'from ',F12.3,XX,F12.3)") X1, Y1
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(24x,'to ',F12.3,XX,F12.3)") X2, Y2
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(28x,'Testing building ',I6)") I
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(28x,'max box ',F12.3,XX,F12.3)") BILDXMAX(I), BILDYMAX(I)
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(28x,'min box ',F12.3,XX,F12.3)") BILDXMIN(I), BILDYMIN(I)
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

C --------- If bounding boxes don't overlap, no intersect possible

	IF( XMAX .LT. BILDXMIN(I) )  GOTO 300
	IF( XMIN .GT. BILDXMAX(I) )  GOTO 300
	IF( YMAX .LT. BILDYMIN(I) )  GOTO 300
	IF( YMIN .GT. BILDYMAX(I) )  GOTO 300

D	IF ( IDEBUG .GT. 0 )THEN
D	  WRITE (KLINE$,"(28x,'Boxes overlap')")
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

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

D	IF ( IDEBUG .GT. 0 )THEN
D	  WRITE (KLINE$,"(28x,'Diagonal for building ',F15.6)") DIAG
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

C --------- Calculate the square of the distance from (XC,YC) to LOS ray

	CALL SQDISPTL ( XC,YC, X1,Y1, X2,Y2, DIST )

D	IF ( IDEBUG .GT. 0 )THEN
D	  WRITE (KLINE$,"(28x,'Distance from centre of building ',F15.6)") DIST
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

C --------- If DIST greater than DIAG, no intersect possible

	IF( DIST .GE. DIAG )  THEN

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'Too far away')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

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

D	IF ( IDEBUG .GT. 0 )THEN
D	  WRITE (KLINE$,"(28x,'This building has ',I6,' nodes')") NUMNODES
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

C --------- Fetch last node of building, set Right/Left flag

	X  =  TRNODESX(LAST)
	Y  =  TRNODESY(LAST)
	IF(  ( ALOS*X + BLOS*Y )  .GT.  CLOS  )  THEN
	  IFLAG  =  1
	ELSE
	  IFLAG  =  2
	ENDIF

C --------- Loop over all outside walls.

	IWALL = 0
	IW = 0

	DO  200  J = NODE1, LAST
	  IW = IW + 1

D	IF ( IDEBUG .GT. 0 )THEN
D	  WRITE (KLINE$,"(28x,'Testing wall ',I6)") IW
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

C ------------- Store previous node

	  XP  =  X
	  YP  =  Y

C ------------- Fetch the coordinates of this node.  The line segment from
C               (XP,YP) to (X,Y) is the wall being tested.

	  X  =  TRNODESX(J)
	  Y  =  TRNODESY(J)

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(28x,'Old nodes ',F12.3,XX,F12.3)") XP, YP
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(28x,'New nodes ',F12.3,XX,F12.3)") X, Y
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

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

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(32x,'Does it intersect? ',I6)") IANS
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IF( IANS .EQ. 0 )  THEN

D	    IF ( IDEBUG .GT. 0 )THEN
D	      WRITE (KLINE$,"(32x,'No')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    GOTO 200
	  ENDIF

D	  IF ( IDEBUG .GT. 0 )THEN
D	    WRITE (KLINE$,"(32x,'Yes')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IWALL = IW

	  GOTO 300

  200	CONTINUE		! END loop over Segments

  300	CONTINUE		! END loop over Buildings

C ----- RETURN to calling routine.

  999	CONTINUE

	RETURN
	END







	 * 
	 */
	
}
