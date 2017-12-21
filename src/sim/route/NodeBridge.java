package sim.route;

public class NodeBridge extends Node {
	/* TODO
	 * Not sure if this is still needed
	 * 
	 * 
	        ELSEIF ( NFLAG .EQ. INODE_BRIDGE ) THEN
	          BRIDGE_ACTION(IUNIT,ISIDE) = INT(SNODE(IFP,IUNIT,ISIDE))

	          CALL SET_UNIT_WINDOW( 2, IUNIT, ISIDE, LUN )
	          CALL ICON_ID( IUNIT, ISIDE, KLINE$ )
	          CALL LINEOUT (1,0,0)
	          WRITE (KLINE$, "('Bridge mode set to')" )
	          CALL LINEOUT (0,0,0)

	          IF ( BRIDGE_ACTION(IUNIT,ISIDE) .EQ. IBRIDGE_DEPLOY) THEN
	            IAVLBDEP_TIME(IUNIT,ISIDE) = NTIME
	            WRITE (KLINE$, "('Deploy bridge')" )
	            CALL LINEOUT (0,0,0)
	          ELSEIF ( BRIDGE_ACTION(IUNIT,ISIDE) .EQ. IBRIDGE_RECOVER) THEN
	            IAVLBDEP_TIME(IUNIT,ISIDE) = 0
	            WRITE (KLINE$, "('Recover bridge')" )
	            CALL LINEOUT (0,0,0)
	          ELSE
	            IAVLBDEP_TIME(IUNIT,ISIDE) = 0
	            WRITE (KLINE$, "('UNKNOWN')" )
	            CALL LINEOUT (0,0,0)
	            BRIDGE_ACTION(IUNIT,ISIDE) = IBRIDGE_RECOVER
	          ENDIF

	          CALL LINEOUT (2,0,0)
	          CALL KDASEL (1)
	          KFLAGNODE(1,IUNIT,ISIDE)    =  INODENORM
	          KTIMENODE(1,IUNIT,ISIDE)    =  0
	 */

}
