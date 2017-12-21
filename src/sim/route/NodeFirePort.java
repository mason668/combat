package sim.route;

public class NodeFirePort extends Node {
	/* TODO
	 * 
	      ELSEIF ( MODE .EQ. INODEFPOINT) THEN
	        NTIME  =  KTIMENODE(IFP,IUNIT,ISIDE)  ! NODE's Time

	        IDEBUG = 0

	        IF ( DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	          WRITE (KLINE$, "('Next node is a fpoint')" )
	          CALL TRACEOUT (0,0,0)
	          WRITE (KLINE$, "('Node time is ',I10,' sec')" ) NTIME
	          CALL TRACEOUT (0,0,0)
	          IDEBUG = 1
	        ENDIF

c	        WRITE (*, "('Next node is a fpoint')" )

	        IBUILD1 = IN_BUILDING(IUNIT,ISIDE)
	        IF ( IBUILD1 .LE. 0 ) THEN
c	          WRITE (*, "('Not in a building')" )
	          GOTO 900
	        ENDIF

	        DX  =  X1 - XOBJ
	        DY  =  Y1 - YOBJ

	        ALOS  =   DY				! Coefficients A, B, and C
	        BLOS  =  -DX				! for the (infinite) LOS line
	        CLOS  =  ALOS*X1 + BLOS*Y1	! in the form Ax + By + C = 0.

	        CALL TRRN_WHICH_WALL ( XOBJ, YOBJ, X1, Y1, ALOS, BLOS, CLOS, IBUILD1, IWALL, IDEBUG )
	        AT_WALL(IUNIT,ISIDE) = IWALL
	        AT_WALL_ANGLE(IUNIT,ISIDE) = ANGLE

	        IF ( IWALL .GT. 0) THEN
c	PRINT *,'WALL ',IWALL
	          XOBJ = X1
	          YOBJ = Y1

	          CALL DELNODE_NOVIEW  ( IUNIT, ISIDE, NXTPT )
	          IF ( IVIEW .GT. 0 ) THEN
	            IF ( NODESTHERE(IVIEW) .EQ. IUNIT ) THEN
	              IDRAW = 0
	              CALL DRAWNODES  ( IUNIT, ISIDE, IVIEW, IDRAW )
	              IDRAW = 1
	              CALL DRAWNODES  ( IUNIT, ISIDE, IVIEW, IDRAW )
	            ENDIF
	          ENDIF

	          IF( NTIME .GT. 0 )  THEN	! Valid
	            KFLAGNODE(1,IUNIT,ISIDE) =  INODESTOP
	            NTIME = NTIME + GAMETIME*60
	            KTIMENODE(1,IUNIT,ISIDE) =  NTIME

	            IF ( DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	              WRITE (KLINE$, "('Current node set to hold until ',I10,' sec')" ) NTIME
	              CALL TRACEOUT (0,0,0)
	            ENDIF

	          ELSE
	            IF ( DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	              WRITE (KLINE$, "('Stoping here')" )
	              CALL TRACEOUT (0,0,0)
	            ENDIF

	            KFLAGNODE(1,IUNIT,ISIDE)  =  INODESTOP
	            KTIMENODE(1,IUNIT,ISIDE)  =   0

	          ENDIF
	        ENDIF

	 */

}
