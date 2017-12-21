package sim.route;

public class NodeCircle extends Node {
	/* TODO
	 * 
	      ELSEIF ( MODE .EQ. INODECIRCLE) THEN
	        NTIME  =  KTIMENODE(IFP,IUNIT,ISIDE)  ! NODE's Time

	        IF ( DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	          WRITE (KLINE$, "('Next node is a CIRCLE node')" )
	          CALL TRACEOUT (0,0,0)
	          WRITE (KLINE$, "('Node time is ',I10,' sec')" ) NTIME
	          CALL TRACEOUT (0,0,0)
	        ENDIF

	        ITYPE = KSYSTYP(IUNIT,ISIDE)

	        CIRCLE_X(IUNIT,ISIDE) = XOBJ
	        CIRCLE_Y(IUNIT,ISIDE) = YOBJ
	        CIRCLE_RADIUS(IUNIT,ISIDE) = SNODE(IFP,IUNIT, ISIDE)
	        CIRCLE_TIME(IUNIT,ISIDE) = FLOAT(NTIME) / 60.0 ! convert to real minutes ie game clock

	        CALL DELNODE_NOVIEW  ( IUNIT, ISIDE, NODE )

	        IF ( IVIEW .GT. 0 ) THEN
	          IF ( NODESTHERE(IVIEW) .EQ. IUNIT ) THEN
	            IDRAW = 0
	            CALL DRAWNODES  ( IUNIT, ISIDE, IVIEW, IDRAW )
	            IDRAW = 1
	            CALL DRAWNODES  ( IUNIT, ISIDE, IVIEW, IDRAW )
	          ENDIF
	        ENDIF

	        XOBJ = X1
	        YOBJ = Y1

	 */

}
