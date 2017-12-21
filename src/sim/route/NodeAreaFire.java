package sim.route;

public class NodeAreaFire extends Node {
	/* TODO pre action
	 * 
	      ELSEIF ( MODE .EQ. INODEAREAFIRE) THEN
	        NTIME  =  KTIMENODE(IFP,IUNIT,ISIDE)  ! NODE's Time

	        IF ( DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	          WRITE (KLINE$, "('Next node is an area fire node')" )
	          CALL TRACEOUT (0,0,0)
	          WRITE (KLINE$, "('Node time is ',I10,' sec')" ) NTIME
	          CALL TRACEOUT (0,0,0)
	        ENDIF

	        ITYPE = KSYSTYP(IUNIT,ISIDE)

	        IRNDS = SNODE(IFP,IUNIT,ISIDE)
	        IFLOOR = IRNDS / 1000
	        IRNDS = MOD(IRNDS,1000)

	        CALL SET_AREA_FIRE  ( IUNIT, ISIDE, XOBJ, YOBJ, IRNDS, IFLOOR )

	        CALL DELNODE_NOVIEW  ( IUNIT, ISIDE, NXTPT )
	        IF ( IVIEW .GT. 0 ) THEN
	          IF ( NODESTHERE(IVIEW) .EQ. IUNIT ) THEN
	            IDRAW = 0
	            CALL DRAWNODES  ( IUNIT, ISIDE, IVIEW, IDRAW )
	            IDRAW = 1
	            CALL DRAWNODES  ( IUNIT, ISIDE, IVIEW, IDRAW )
	          ENDIF
	        ENDIF

	        XOBJ  =  X1
	        YOBJ  =  Y1

	 */

}
