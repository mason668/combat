package sim.route;

public class NodePit extends Node {
	/* TODO
	 * 
	      ELSEIF ( MODE .EQ. INODEPIT) THEN
	        NTIME  =  KTIMENODE(IFP,IUNIT,ISIDE)  ! NODE's Time

	        IF ( DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	          WRITE (KLINE$, "('Next node is a PIT node')" )
	          CALL TRACEOUT (0,0,0)
	          WRITE (KLINE$, "('Node time is ',I10,' sec')" ) NTIME
	          CALL TRACEOUT (0,0,0)
	        ENDIF

	        ITYPE = KSYSTYP(IUNIT,ISIDE)

	        IPIT = SNODE(IFP, IUNIT, ISIDE)
	        IF ( IPIT .GT. 0 ) THEN

	          DX = XUNIT(IUNIT,ISIDE) - PIT_X(IPIT)
	          DY = YUNIT(IUNIT,ISIDE) - PIT_Y(IPIT)

	          DIST = SQRT((DX*DX)+(DY*DY))

	          IF ( DIST .LE. RNGPIT_INNER) THEN
	            IF ( PIT_OCCUPANT(IPIT) .LE. 0 ) THEN ! if pit unoccupied, mount it otherwise just wait here
	              CALL OCCUPY_PIT (IUNIT, ISIDE, IPIT)
	              CALL DELNODE_NOVIEW  ( IUNIT, ISIDE, NODE )
	              IF ( IVIEW .GT. 0 ) THEN
	                IF ( NODESTHERE(IVIEW) .EQ. IUNIT ) THEN
	                  IDRAW = 0
	                  CALL DRAWNODES  ( IUNIT, ISIDE, IVIEW, IDRAW )
	                  IDRAW = 1
	                  CALL DRAWNODES  ( IUNIT, ISIDE, IVIEW, IDRAW )
	                ENDIF
	              ENDIF
	            ENDIF

	            XOBJ = X1
	            YOBJ = Y1
	            GOTO 900

	          ENDIF

	          XOBJ = PIT_X(IPIT)
	          YOBJ = PIT_Y(IPIT)

	          GOTO 900
	        ENDIF
	 */

}
