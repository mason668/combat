package sim.route;

public class NodeWaterSOP extends Node {
	/* TODO
	 * 
	        ELSEIF ( NFLAG .EQ. INODE_WATER ) THEN
	          ON_WATER(IUNIT,ISIDE) = INT(SNODE(IFP,IUNIT,ISIDE))

	          CALL SET_UNIT_WINDOW( 2, IUNIT, ISIDE, LUN )
	          CALL ICON_ID( IUNIT, ISIDE, KLINE$ )
	          CALL LINEOUT (1,0,0)
	          WRITE (KLINE$, "('Water crossing mode set to')" )
	          CALL LINEOUT (0,0,0)

	          IF ( ON_WATER(IUNIT,ISIDE) .EQ. IWATER_BRIDGE) THEN
	            IAVLBDEP_TIME(IUNIT,ISIDE) = NTIME
	            WRITE (KLINE$, "('Deploy bridge')" )
	            CALL LINEOUT (0,0,0)
	          ELSEIF ( ON_WATER(IUNIT,ISIDE) .EQ. IWATER_STOP) THEN
	            IAVLBDEP_TIME(IUNIT,ISIDE) = 0
	            WRITE (KLINE$, "('Stop')" )
	            CALL LINEOUT (0,0,0)
	          ELSEIF ( ON_WATER(IUNIT,ISIDE) .EQ. IWATER_SWIM) THEN
	            IAVLBDEP_TIME(IUNIT,ISIDE) = 0
	            WRITE (KLINE$, "('Swim')" )
	            CALL LINEOUT (0,0,0)
	          ELSEIF ( ON_WATER(IUNIT,ISIDE) .EQ. IWATER_FORD) THEN
	            IAVLBDEP_TIME(IUNIT,ISIDE) = 0
	            WRITE (KLINE$, "('Ford')" )
	            CALL LINEOUT (0,0,0)
	          ELSEIF ( ON_WATER(IUNIT,ISIDE) .EQ. IWATER_XBRIDGE) THEN
	            IAVLBDEP_TIME(IUNIT,ISIDE) = 0
	            WRITE (KLINE$, "('Cross bridge')" )
	            CALL LINEOUT (0,0,0)
	          ELSE
	            IAVLBDEP_TIME(IUNIT,ISIDE) = 0
	            WRITE (KLINE$, "('Stop')" )
	            CALL LINEOUT (0,0,0)
	            ON_WATER(IUNIT,ISIDE) = IWATER_STOP
	          ENDIF

	          CALL LINEOUT (2,0,0)
	          CALL KDASEL (1)
	          KFLAGNODE(1,IUNIT,ISIDE)    =  INODENORM
	          KTIMENODE(1,IUNIT,ISIDE)    =  0
	 */

}
