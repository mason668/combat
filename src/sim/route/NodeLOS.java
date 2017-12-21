package sim.route;

import data.map.Coordinate;
import sim.Scenario;
import sim.entity.MoverEntity;

public class NodeLOS extends Node {
	public boolean waiting(MoverEntity entity, Scenario scenario){
		//TODO do the LOS check
		return true;
	}

	/*TODO
	 * 
	      IF ( MODE .EQ. INODELOS) THEN
	        NTIME  =  KTIMENODE(IFP,IUNIT,ISIDE)  ! NODE's Time

	        IF ( DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	          WRITE (KLINE$, "('Next node is a LOS node')" )
	          CALL TRACEOUT (0,0,0)
	          WRITE (KLINE$, "('Node time is ',I10,' sec')" ) NTIME
	          CALL TRACEOUT (0,0,0)
	        ENDIF

	        ITYPE = KSYSTYP(IUNIT,ISIDE)
	        CALL UNITXY ( IUNIT,ISIDE, XO,YO )
	        CALL UNIT_SENSOR_HEIGHT_AGL(IUNIT, ISIDE, ZO)

	        IBUILD = IN_BUILDING(IUNIT,ISIDE)
	        IWALL = AT_WALL(IUNIT,ISIDE)
	        IROOF = ON_ROOF(IUNIT,ISIDE)
	        IFLOOR = ON_FLOOR(IUNIT,ISIDE)

	        JBUILD = 0
	        JWALL = 0
	        JROOF = 0
	        JFLOOR = 0

	        CALL DOLOS_BUILD ( XO,YO,ZO, XOBJ,YOBJ,ZOBJ, PLOS,
     *			IBUILD, IWALL, JBUILD, JWALL, IROOF, JROOF, IFLOOR, JFLOOR )

	        IF ( DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	          WRITE (KLINE$, "('PLOS to target point is ',F6.3)" ) PLOS
	          CALL TRACEOUT (0,0,0)
	        ENDIF

	        IF ( PLOS .GT. 0.1 ) THEN
	          XOBJ = X1
	          YOBJ = Y1

	          IF ( DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	            WRITE (KLINE$, "('No need to move closer')" )
	            CALL TRACEOUT (0,0,0)
	          ENDIF

	          IF( NTIME .GT. 0 )  THEN	! Valid
	            KFLAGNODE(1,IUNIT,ISIDE) =  INODESTOP
	            NTIME = NTIME + GAMETIME*60
	            KTIMENODE(1,IUNIT,ISIDE) =  NTIME

	            IF ( DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	              WRITE (KLINE$, "('Current node set to hold until ',I10,' sec')" ) NTIME
	              CALL TRACEOUT (0,0,0)
	            ENDIF

	            CALL DELNODE_NOVIEW  ( IUNIT, ISIDE, NODE )
	            IF ( IVIEW .GT. 0 ) THEN
	              IF ( NODESTHERE(IVIEW) .EQ. IUNIT ) THEN
	                IDRAW = 0
	                CALL DRAWNODES  ( IUNIT, ISIDE, IVIEW, IDRAW )
	                IDRAW = 1
	                CALL DRAWNODES  ( IUNIT, ISIDE, IVIEW, IDRAW )
	              ENDIF
	            ENDIF
	          ELSE
	            IF ( DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	              WRITE (KLINE$, "('Holding here')" )
	              CALL TRACEOUT (0,0,0)
	            ENDIF

	          ENDIF

	        ENDIF

	        GOTO 900

	        IF( NTIME.LE.0 .OR. NTIME.GT.GAMETIME*60 )  THEN	! Valid

	          ITYPE = KSYSTYP(IUNIT,ISIDE)

	          CALL UNITXY ( IUNIT,ISIDE, XO,YO )
	          CALL UNIT_SENSOR_HEIGHT_AGL(IUNIT, ISIDE, ZO)

	          IBUILD = IN_BUILDING(IUNIT,ISIDE)
	          IWALL = AT_WALL(IUNIT,ISIDE)
	          IROOF = ON_ROOF(IUNIT,ISIDE)

	          JBUILD = 0
	          JWALL = 0
	          JROOF = 0

	          CALL DOLOS_BUILD ( XO,YO,ZO, XOBJ,YOBJ,ZOBJ, PLOS,
     *			IBUILD, IWALL, JBUILD, JWALL, IROOF, JROOF )

	          IF ( DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	            WRITE (KLINE$, "('PLOS to target point is ',F6.3)" ) PLOS
	            CALL TRACEOUT (0,0,0)
	          ENDIF

	          IF ( PLOS .GT. 0.1 ) THEN
	            XOBJ = X1
	            YOBJ = Y1

	            IF ( DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	              WRITE (KLINE$, "('No need to move closer')" )
	              CALL TRACEOUT (0,0,0)
	            ENDIF

	          ENDIF
	        ELSE ! time has expired
	          IF ( DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	            WRITE (KLINE$, "('Time has expired')" )
	            CALL TRACEOUT (0,0,0)
	          ENDIF

	          CALL DELNODE  ( IUNIT, ISIDE, IVIEW, NODE )
	          XOBJ = X1
	          YOBJ = Y1

	        ENDIF

	 */

}
