package sim.route;

public class NodeMineSOP extends Node {
	/* TODO
	 * 
	        ELSEIF ( NFLAG .EQ. INODE_MINESOP ) THEN
	          ON_MINE_SOP(IUNIT,ISIDE) = INT(SNODE(IFP,IUNIT,ISIDE))

	          CALL SET_UNIT_WINDOW( 2, IUNIT, ISIDE, LUN )
	          CALL ICON_ID( IUNIT, ISIDE, KLINE$ )
	          CALL LINEOUT (1,0,0)
	          WRITE (KLINE$, "('Mine SOP set to')" )
	          CALL LINEOUT (0,0,0)

	          IF ( ON_MINE_SOP(IUNIT,ISIDE) .EQ. 0) THEN
	            WRITE (KLINE$, "('Stop at mine')" )
	            CALL LINEOUT (0,0,0)
	          ELSEIF ( ON_MINE_SOP(IUNIT,ISIDE) .EQ. 1) THEN
	            WRITE (KLINE$, "('Start breaching at mine')" )
	            CALL LINEOUT (0,0,0)
	          ELSEIF ( ON_MINE_SOP(IUNIT,ISIDE) .EQ. 2) THEN
	            WRITE (KLINE$, "('Do not Stop')" )
	            CALL LINEOUT (0,0,0)
	          ELSEIF ( ON_MINE_SOP(IUNIT,ISIDE) .EQ. 3) THEN
	            WRITE (KLINE$, "('GP stop')" )
	            CALL LINEOUT (0,0,0)
	          ELSEIF ( ON_MINE_SOP(IUNIT,ISIDE) .EQ. 4) THEN
	            WRITE (KLINE$, "('FC stop')" )
	            CALL LINEOUT (0,0,0)
	          ELSE
	            WRITE (KLINE$, "('UNKNOWN')" )
	            CALL LINEOUT (0,0,0)
	            ON_MINE_SOP(IUNIT,ISIDE) = 0
	          ENDIF

	          CALL LINEOUT (2,0,0)
	          CALL KDASEL (1)
	          KFLAGNODE(1,IUNIT,ISIDE)    =  INODENORM
	          KTIMENODE(1,IUNIT,ISIDE)    =  0

	 */

}
