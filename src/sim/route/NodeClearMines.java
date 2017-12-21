package sim.route;

public class NodeClearMines extends Node {
	/* TODO
	 * 
	        ELSEIF ( NFLAG .EQ. INODE_MFCLEAR ) THEN
	          IBREACH_MINES(IUNIT,ISIDE) = INT(SNODE(IFP,IUNIT,ISIDE))

	          CALL SET_UNIT_WINDOW( 2, IUNIT, ISIDE, LUN )
	          CALL ICON_ID( IUNIT, ISIDE, KLINE$ )
	          CALL LINEOUT (1,0,0)
	          WRITE (KLINE$, "('Lane mode set to')" )
	          CALL LINEOUT (0,0,0)

	          IF ( IBREACH_MINES(IUNIT,ISIDE) .EQ. 0) THEN
	            WRITE (KLINE$, "('No clear')" )
	            CALL LINEOUT (0,0,0)
	          ELSEIF ( IBREACH_MINES(IUNIT,ISIDE) .EQ. 1) THEN
	            WRITE (KLINE$, "('Clear')" )
	            CALL LINEOUT (0,0,0)
	          ELSEIF ( IBREACH_MINES(IUNIT,ISIDE) .EQ. 2) THEN
	            WRITE (KLINE$, "('Prove')" )
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
	        ENDIF
	 */

}
