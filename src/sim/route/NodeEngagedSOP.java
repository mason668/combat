package sim.route;

public class NodeEngagedSOP extends Node {
	/* TODO
	 * 
	        ELSEIF ( NFLAG .EQ. INODE_ENGAGED ) THEN
	          ON_ENGAGED(IUNIT,ISIDE) = INT(SNODE(IFP,IUNIT,ISIDE))

	          CALL SET_UNIT_WINDOW( 2, IUNIT, ISIDE, LUN )
	          CALL ICON_ID( IUNIT, ISIDE, KLINE$ )
	          CALL LINEOUT (1,0,0)
	          WRITE (KLINE$, "('Engaged SOP set to')" )
	          CALL LINEOUT (0,0,0)

	          IF ( ON_ENGAGED(IUNIT,ISIDE) .EQ. 0) THEN
	            WRITE (KLINE$, "('Nothing')" )
	            CALL LINEOUT (0,0,0)
	          ELSEIF ( ON_ENGAGED(IUNIT,ISIDE) .EQ. 1) THEN
	            WRITE (KLINE$, "('Stop')" )
	            CALL LINEOUT (0,0,0)
	          ELSEIF ( ON_ENGAGED(IUNIT,ISIDE) .EQ. 2) THEN
	            WRITE (KLINE$, "('Dismount')" )
	            CALL LINEOUT (0,0,0)
	          ELSEIF ( ON_ENGAGED(IUNIT,ISIDE) .EQ. 2) THEN
	            WRITE (KLINE$, "('Drop')" )
	            CALL LINEOUT (0,0,0)
	          ELSEIF ( ON_ENGAGED(IUNIT,ISIDE) .EQ. 2) THEN
	            WRITE (KLINE$, "('Pop Smoke')" )
	            CALL LINEOUT (0,0,0)
	          ELSEIF ( ON_ENGAGED(IUNIT,ISIDE) .EQ. 2) THEN
	            WRITE (KLINE$, "('Face')" )
	            CALL LINEOUT (0,0,0)
	          ELSE
	            WRITE (KLINE$, "('UNKNOWN')" )
	            CALL LINEOUT (0,0,0)
	            IBREACH_MINES(IUNIT,ISIDE) = 0
	          ENDIF

	          CALL LINEOUT (2,0,0)
	          CALL KDASEL (1)
	          KFLAGNODE(1,IUNIT,ISIDE)    =  INODENORM
	          KTIMENODE(1,IUNIT,ISIDE)    =  0

	 */

}
