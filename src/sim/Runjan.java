package sim;

public class Runjan {
	/*
	 * 
	SUBROUTINE  RUNJAN  ( IRESET )

	IMPLICIT NONE

	include 'global.f'
	include 'globunits.f'
	include 'globload.f'	! this should be removed soon too
c	include 'globsrch.f'	! so should this
	include 'globmove.f'
	include 'glbsensr.f'
	include 'globsetup.f'
	include 'globdetect.f'
	include 'globrpt.f'
	include 'globnet.f'

	REAL		SUPRMIN, DTSEARCH, TT3, TIMEZERO, SECNDS, TIMELAST
	REAL		TUNELAST, TUNEZERO, CKPTTIM, TEST, GAMECLOCK, GAMEDELTA
	REAL		SYNCHDELTA, WALLDELTA, ETIME, TNEXT_SUPR, TNEXT_MOVE, TNEXT_FIRE
	INTEGER*4	I, ICNT, I1, J, NPROC, JSIDE, IENEMY, ITUNE, ISEE, NONMY
	INTEGER*4	ISENS, ISTYP, IBAND, IRVIEW, IRESET, ITYPE

	REAL		DTHEAT
	INTEGER*4	NUMBRACKETS

C------------------------------------------------------------------------------
	PARAMETER   (DTHEAT  = 1.0)		! DOHEAT   interval = 1 Min

	CHARACTER*12	TIMESTRING$
        CHARACTER*24    LONGTIME$                     ! UNIX

	REAL	SMALLTIME, CTICK
	REAL	ONE_SECOND

	INTEGER*4	IUNIT, ISIDE, ISTART, ILAST

	PARAMETER	(NUMBRACKETS = 5)
	INTEGER*4	IBEG(NUMSIDES,NUMBRACKETS),
     *			IEND(NUMSIDES,NUMBRACKETS),
     *			IBRACKET

C------------------------------------------------------------------------------

	ONE_SECOND = 1.0 / 60.0

	IF( PERIOD .GT. 0.001 )  THEN
	    IF( .NOT. CLOCK_TICKING )  TNEXT(3) = 0.0
	ELSE
	    TNEXT(3)  =  TNEVER * 100
	ENDIF
	TNEXT(12)  =  TNEVER * 100

	TNEXT(4) = STARTTIME + 0.1 ! TNEVER
	TNEXT(4) = TNEVER * 100

	SMALLTIME = (1.0 / 60.0) * 0.5 ! * 0.1

	SUPRMIN = 5 / 60.0 ! SMALLTIME

	CTICK = DTSEARCH

c	CALL INIT_BRACKETS()

	DO I=1, NUMSIDES
	  IF ( KNUMUNITS(I) .GT. NUMBRACKETS ) THEN
	    ICNT = KNUMUNITS(I) / NUMBRACKETS
	    I1 = 1
	    DO J=1,NUMBRACKETS
	      IBEG(I,J) = I1
	      I1 = I1 + ICNT 
	      IEND(I,J) = I1 - 1
	    ENDDO
	    IEND(I,NUMBRACKETS) = KNUMUNITS(I)
	  ELSE
	    DO J=1,NUMBRACKETS
	      IBEG(I,J) = 0
	      IEND(I,J) = 0
	    ENDDO
	    IBEG(I,1) = 1
	    IEND(I,NUMBRACKETS) = KNUMUNITS(I)
	  ENDIF
	ENDDO

	IBRACKET = 0

C------------------------------------------------------------------------------
C------------------------------------------------------------------------------

	TT3  =  0.0

C------ Init Wall Clock & Last Event Time

	TIMEZERO  =  SECNDS( 0.0 )
	TIMELAST  =  CLOCK * 60.0		! Convert CLOCK to seconds
c	SYNCHZERO = TIMEZERO
c	SYNCHLAST = TIMELAST

	TUNELAST = TIMELAST
	TUNEZERO = TIMEZERO

C---------- Get the time and date.

	write (IFILELOG,"('Janus battle mode')")
        CALL GETTIMEF ( LONGTIME$ )
	write (IFILELOG,"('Game starts at time ',A24)") LONGTIME$
	write (IFILELOG,"('Checkpoint freq = ',F12.5)") CKPTFREQ
	write (IFILELOG,"('Clock = ',F12.5)") CLOCK
	write (IFILELOG,"('kreset = ',I4)") KRESET
	write (IFILELOG,"('ireset = ',I4)") IRESET

C------ Check if Automatic Checkpoint Save has been requested

	IF( CKPTFREQ .GT. 0.0 )  THEN
	    CKPTTIM  =  CLOCK + CKPTFREQ
	ELSE
	  CKPTTIM = TNEVER
	ENDIF

C ---- Initializer the position array (THN 7-MAY-2001)
         DO I = 1, NUMSIDES
            DO J = 1, NUMUNITS
               UNITARRAY(I, J, 1) = -1
               UNITARRAY(I, J, 2) = -1
            ENDDO
         ENDDO

C ---- Send the initial positions of all the units on the network (THN 7-MAY-2001)
	IF( NET_CONNECT .AND. NET_MOVE) THEN
	   PRINT *, 'Sending unit initial positions'
	   DO  ISIDE = 1, NUMSIDES
	      ISTART = 1
	      ILAST  =  KNUMUNITS(ISIDE)
	      DO  IUNIT = ISTART, ILAST
C ---- Send the unit's position, but if the network fails, don't do a disk save.
		 CALL WRITNETMOVE(IUNIT, ISIDE, 0)
	      ENDDO
	   ENDDO
	   PRINT *, 'Finished'
	ENDIF

C ---- Send the initail ammo and fuel reports.
	CALL WTSTATUS

C ---- Initialise STATFREQ, the variable to check how often the status reports are printed.
	NET_STATFREQ = CLOCK + NET_STATUSRPT
c	PRINT *, 'Next Status Report at ', NET_STATFREQ

C..............................................................................
C
C
C				Main event loop
C
C..............................................................................

	DO WHILE  ( KRESET .EQ. 0 )

C------------------------------------------------------------------------------
C
C	If the player has exited the simulation then 'KRESET' is
C	set to 1 in MENU:FUNCBUT2.
C
C------------------------------------------------------------------------------

C------ Call routine to do interactive planning.

	CALL DOPLAN

	IF ( CLOCK_PAUSED ) THEN
	  GOTO 600
	ENDIF

C---------------------------- Find the next scheduled "non planning" event.

	TEST  =  TNEVER
	DO  I = 1, 13
	  IF( TNEXT(I) .LT. TEST )  THEN
	    TEST   =  TNEXT(I)
	    NPROC  =  I
	  ENDIF
	ENDDO

D	IF (DEBUGCYCL .GT. 0 ) THEN
D	  WRITE (KLINE$, "('Next event type = ',I2)") NPROC
D	  CALL TRACEOUT (0,2,0)
D	ENDIF

	CLOCK    =  TNEXT(NPROC)



c	write (IFILELOG,"('Clock = ',F12.5, I4)") CLOCK, NPROC



D	IF (DEBUGCYCL .GT. 0 ) THEN
D	  WRITE (KLINE$, "('Clock = ',F12.4)") CLOCK
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

C------ Check if we need to slow down the simulation (to Real-time)

	GAMECLOCK  =  CLOCK * 60.0		! Convert CLOCK to seconds
	GAMEDELTA  =  GAMECLOCK - TIMELAST	! Elapsed game time (sec)
	SYNCHDELTA =  GAMECLOCK - SYNCHLAST

	IF( GAMEDELTA .GE. 2.0 )  THEN
	  ITUNE = 1
	  IF( REALTIME .GT. 0.0 )  THEN
10	    CONTINUE
	    WALLDELTA  =  SECNDS( TIMEZERO )	! Seconds since
							! TIMEZERO (real)
	    WALLDELTA  =  WALLDELTA * REALTIME	! Times speed factor
	    IF( GAMEDELTA .GT. WALLDELTA )  THEN

c	      IF ( ITUNE .GT. 0 ) THEN ! try tuning the system
c	        TUNEDELTA = SECNDS(TUNEZERO)
c	        IF ( TUNEDELTA .GT. 300.0 ) THEN ! only try tuning every 5 mins or so
c	          RATIO = GAME_DELTA / WALLDELTA
c	          IF ( RATIO .GT. 2 ) THEN
c	            IF ( MAXTRGTS .GT. 5 ) THEN
c	              MAXTRGTS = MAXTRGTS - 1
c	            ENDIF
c	          ELSEIF ( RATIO .GT. 0 ) THEN
c	            DTSEARCH = DTSEARCH + ONE_SECOND
c	          ELSEIF ( RATIO .LT. 0 ) THEN
c	            IF ( DTSEARCH .GT. ONE_SECOND ) THEN
c	              DTSEARCH = DTSEARCH - ONE_SECOND
c	            ENDIF
c	          ENDIF
c	          TUNELAST = GAME_CLOCK
c	          TUNEZERO = SECNDS(0.0)
c	        ENDIF
c	        ITUNE = 0
c	      ENDIF

	      WALLDELTA  =  SECNDS( SYNCHZERO )
	      WALLDELTA  =  WALLDELTA * REALTIME
	      IF ( SYNCHDELTA .GT. WALLDELTA) THEN
C		CALL WAIT ( 0.010 )
	        CALL DOPLAN
	        IF( REALTIME .GT. 0.0 )  GOTO 10
	      ENDIF
	    ENDIF
	  ENDIF
	  TIMEZERO  =  SECNDS( 0.0 )		! Set new TIMEZERO
	  TIMELAST  =  GAMECLOCK		! Save Event time (sec)
	ENDIF

	GOTO (  101, 102, 103, 104, 105, 106, 107,
     *		     108, 109, 110, 111, 112, 113   )  NPROC

C------------------------------------------------------------------------------

C---------------------- Process "MOVEMENT"
C---------------------- Update unit positions due to movement

 101	CONTINUE

	CALL DOPLAN

D	IF (DEBUGCYCL .GT. 0 ) THEN
D	  WRITE (KLINE$, "('General icon events')")
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$, "('Clock ',F12.3)") CLOCK
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IBRACKET = MOD(IBRACKET + 1,NUMBRACKETS)
	IF (IBRACKET .EQ. 0 ) IBRACKET = NUMBRACKETS

c	IF ( DOEVENTQUE .GT. 0 ) THEN

D	  IF (DEBUGCYCL .GT. 0 ) THEN
D	    WRITE (KLINE$, "('Checking resupply')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

c	  CALL TEST_MOVE

c	  TIMESTAMP1  =  SECNDS( 0.0 )

	  CALL GET_NEXT_RESUPPLY (IUNIT, ISIDE, ETIME)
	  DO WHILE (ETIME .LE. CLOCK .AND. IUNIT .GT. 0)
C	    TNEXT_ = TIMOVE(IUNIT,ISIDE)

D	    IF (DEBUGCYCL .GT. 0 ) THEN
D	      WRITE (KLINE$, "(4X,'Icon ',I6,x,I2)") IUNIT, ISIDE
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Time ',F12.3)") ETIME
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    CALL SET_T2RESUPPLY(IUNIT, ISIDE, TNEXT_MOVE)
	    CALL GET_NEXT_RESUPPLY (IUNIT, ISIDE, ETIME)
	  ENDDO

c	  TIMESTAMP1  =  SECNDS( 0.0 )
c	  TIMESTAMP2  =  SECNDS( 0.0 )
c	  write (IFILELOG,"(F12.3,' Movement  start ',f15.4,' end ',f15.4)") CLOCK, TIMESTAMP1, TIMESTAMP2

C	**********************************************************

D	  IF (DEBUGCYCL .GT. 0 ) THEN
D	    WRITE (KLINE$, "('Checking suppression')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  CALL GET_NEXT_SUPP (IUNIT, ISIDE, ETIME)
	  DO WHILE (ETIME .LE. CLOCK .AND. IUNIT .GT. 0)

	    TNEXT_SUPR = T2SUPR(IUNIT,ISIDE)

D	    IF (DEBUGCYCL .GT. 0 ) THEN
D	      WRITE (KLINE$, "(4X,'Icon ',I6,x,I2)") IUNIT, ISIDE
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Time ',F12.3)") ETIME
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Icon time ',F12.3)") TNEXT_SUPR
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    IF ( TSUPRS(IUNIT,ISIDE) .GT. 0.0 ) THEN

D	      IF (DEBUGSUPR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	        CALL ICON_ID( IUNIT, ISIDE, KLINE$)
D	        CALL TRACEOUT (0,2,0)
D	        WRITE (KLINE$,"('Suppression Recovery.')")
D	        CALL TRACEOUT (0,0,0)
D	        WRITE (KLINE$,"('Old suppression amount ',F6.2,'%')")
D     *			TSUPRS(IUNIT,ISIDE) * 100
D	        CALL TRACEOUT (0,0,0)
D	      ENDIF

	      IF ( KSPRINT(IUNIT,ISIDE) .NE. MOVRUN ) THEN
	        TSUPRS(IUNIT,ISIDE) = TSUPRS(IUNIT,ISIDE) - SUPRMIN
	        LAST_SUPPRESSED(IUNIT,ISIDE) = CLOCK
	      ELSE

D	        IF (DEBUGSUPR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	          WRITE (KLINE$,"('Running, unable to recover')")
D	          CALL TRACEOUT (0,0,0)
D	        ENDIF

	      ENDIF

D	      IF (DEBUGSUPR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	        WRITE (KLINE$,"('New suppression amount ',F6.2,'%')")
D     *			TSUPRS(IUNIT,ISIDE) * 100
D	        CALL TRACEOUT (0,0,0)
D	      ENDIF

	    ELSE
	      TSUPRS(IUNIT,ISIDE) = 0.0
	      IF ( LAST_SUPPRESSED(IUNIT,ISIDE) .LT. (CLOCK - 3.0) ) THEN

D	        IF (DEBUGSUPR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	          WRITE (KLINE$,"('Attempting to reload consumables ')")
D	          CALL TRACEOUT (0,0,0)
D	        ENDIF

	        CALL RELOAD_GRENADES ( IUNIT, ISIDE)
	        CALL RELOAD_POINT_DEFENCE ( IUNIT, ISIDE)
	        CALL READY_AMMO ( IUNIT, ISIDE)
	      ENDIF

	    ENDIF

c	IF ( KSPRINT(IUNIT,ISIDE) .EQ. MOVRUN ) THEN


	    IF ( NSCORE(IUNIT,ISIDE) .GT. 0 ) THEN
	      TNEXT_SUPR = TNEXT_SUPR + SUPRMIN
	    ELSE
	      TNEXT_SUPR = TNEVER
	    ENDIF

	    CALL SET_T2SUPP(IUNIT, ISIDE, TNEXT_SUPR)

	    CALL GET_NEXT_SUPP (IUNIT, ISIDE, ETIME)
	  ENDDO

C	**********************************************************

D	  IF (DEBUGCYCL .GT. 0 ) THEN
D	    WRITE (KLINE$, "('Checking movement')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

c	  CALL TEST_MOVE

c	  TIMESTAMP1  =  SECNDS( 0.0 )

	  CALL GET_NEXT_MOVE (IUNIT, ISIDE, ETIME)

c	write (IFILELOG,"('Processing icon movement ',I4, I4,F12.5,f12.5)") IUNIT, ISIDE, etime, clock

	  DO WHILE (ETIME .LE. CLOCK .AND. IUNIT .GT. 0)
	    TNEXT_MOVE = TIMOVE(IUNIT,ISIDE)

D	    IF (DEBUGCYCL .GT. 0 ) THEN
D	      WRITE (KLINE$, "(4X,'Icon ',I6,x,I2)") IUNIT, ISIDE
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Time ',F12.3)") ETIME
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Icon time ',F12.3)") TNEXT_MOVE
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    CALL MOVEMENT( IUNIT, ISIDE, TNEXT_MOVE, DSTMOVE )
	    CALL SET_T2MOVE(IUNIT, ISIDE, TNEXT_MOVE)
	    CALL DOPLAN
	    CALL GET_NEXT_MOVE (IUNIT, ISIDE, ETIME)
	  ENDDO

c	  TIMESTAMP1  =  SECNDS( 0.0 )
c	  TIMESTAMP2  =  SECNDS( 0.0 )
c	  write (IFILELOG,"(F12.3,' Movement  start ',f15.4,' end ',f15.4)") CLOCK, TIMESTAMP1, TIMESTAMP2

C	**********************************************************

D	  IF (DEBUGCYCL .GT. 0 ) THEN
D	    WRITE (KLINE$, "('Checking target gather enemy')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

c	  TIMESTAMP1  =  SECNDS( 0.0 )

	  CALL GET_NEXT_SCAN (IUNIT, ISIDE, ETIME)

	  DO WHILE (ETIME .LE. CLOCK .AND. IUNIT .GT. 0)

c	write (IFILELOG,"('Processing icon target gather ',I4, I4,F12.5,f12.5)") IUNIT, ISIDE, etime, clock

D	    IF (DEBUGCYCL .GT. 0 ) THEN
D	      WRITE (KLINE$, "(4X,'Icon ',I6,x,I2)") IUNIT, ISIDE
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Time ',F12.3)") ETIME
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Icon time ',F12.3)") T2SCAN(IUNIT, ISIDE)
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    IFORCE = MYFORCE(IUNIT,ISIDE)

	    JSIDE = 3-ISIDE

	    IENEMY = 1

	    ISEE = 0
	    CALL TARGET_GATHER (IUNIT,ISIDE, JSIDE, ISEE)
	    ITYPE = KSYSTYP(IUNIT,ISIDE)

	    IF (NSCORE(IUNIT,ISIDE) .GT. 0 ) THEN
	      IF ( FLYERS(ITYPE) .GT. 0 ) THEN
	        CALL GET_SEARCH_TIME  ( IFORCE, IENEMY, CTICK )
	        T2SCAN(IUNIT,ISIDE) = CLOCK + CTICK
	      ELSE
	        CALL GET_SCAN_TIME  ( IFORCE, IENEMY, CTICK )
	        T2SCAN(IUNIT,ISIDE) = CLOCK + CTICK
	      ENDIF
	    ELSE
	      T2SCAN(IUNIT,ISIDE) = TNEVER
	    ENDIF

	    CALL ALTER_SCAN(IUNIT, ISIDE )
	    CALL DOPLAN
	    CALL GET_NEXT_SCAN (IUNIT, ISIDE, ETIME)
	  ENDDO

c	  TIMESTAMP2  =  SECNDS( 0.0 )
c	  write (IFILELOG,"(F12.3,' Target    start ',f15.4,' end ',f15.4)") CLOCK, TIMESTAMP1, TIMESTAMP2

C	**********************************************************

D	  IF (DEBUGCYCL .GT. 0 ) THEN
D	    WRITE (KLINE$, "('Checking target gather friends')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

c	  TIMESTAMP1  =  SECNDS( 0.0 )

	  CALL GET_NEXT_SCANF (IUNIT, ISIDE, ETIME)
	  DO WHILE (ETIME .LE. CLOCK .AND. IUNIT .GT. 0)

D	    IF (DEBUGCYCL .GT. 0 ) THEN
D	      WRITE (KLINE$, "(4X,'Icon ',I6,x,I2)") IUNIT, ISIDE
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Time ',F12.3)") ETIME
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Icon time ',F12.3)") T2SCAN(IUNIT, ISIDE)
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    IFORCE = MYFORCE(IUNIT,ISIDE)

	    JSIDE = ISIDE

	    IENEMY = 2

	    ISEE = 0
	    CALL TARGET_GATHER (IUNIT,ISIDE, JSIDE, ISEE)
	    ITYPE = KSYSTYP(IUNIT,ISIDE)

	    IF (NSCORE(IUNIT,ISIDE) .GT. 0 ) THEN
	      IF ( FLYERS(ITYPE) .GT. 0 ) THEN
	        CALL GET_SEARCH_TIME  ( IFORCE, IENEMY, CTICK )
	        T2SCANF(IUNIT,ISIDE) = CLOCK + CTICK
	      ELSE
	        CALL GET_SCAN_TIME  ( IFORCE, IENEMY, CTICK )
	        T2SCANF(IUNIT,ISIDE) = CLOCK + CTICK
	      ENDIF
	    ELSE
	      T2SCANF(IUNIT,ISIDE) = TNEVER
	    ENDIF

	    CALL ALTER_SCANF(IUNIT, ISIDE )
	    CALL DOPLAN
	    CALL GET_NEXT_SCANF (IUNIT, ISIDE, ETIME)
	  ENDDO

c	  TIMESTAMP2  =  SECNDS( 0.0 )
c	  write (IFILELOG,"(F12.3,' Target    start ',f15.4,' end ',f15.4)") CLOCK, TIMESTAMP1, TIMESTAMP2

C	**********************************************************

D	  IF (DEBUGCYCL .GT. 0 ) THEN
D	    WRITE (KLINE$, "('Checking detection vs enemy')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

c	  TIMESTAMP1  =  SECNDS( 0.0 )

	  CALL GET_NEXT_DTEC (IUNIT, ISIDE, ETIME)

c	write (IFILELOG,"('Processing icon enemy detection ',I4, I4,F12.5,f12.5)") IUNIT, ISIDE, etime, clock

	  DO WHILE (ETIME .LE. CLOCK .AND. IUNIT .GT. 0)

D	    IF (DEBUGCYCL .GT. 0 ) THEN
D	      WRITE (KLINE$, "(4X,'Icon ',I6,x,I2)") IUNIT, ISIDE
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Time ',F12.3)") ETIME
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Icon time ',F12.3)") T2SEARCH(IUNIT, ISIDE)
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    NONMY = 0
	    IFORCE = MYFORCE(IUNIT,ISIDE)

	    JSIDE = 3-ISIDE

	    IENEMY = 1
	    CALL GET_SEARCH_TIME  ( IFORCE, IENEMY, CTICK )

	    CALL DETECTIONS ( IUNIT, ISIDE, NONMY, CTICK, JSIDE )
	    ISEETRGTS(IUNIT,ISIDE,JSIDE) = NONMY

	    ISEENMY( IUNIT,ISIDE) = ISEETRGTS(IUNIT,ISIDE,1) +
     *		ISEETRGTS(IUNIT,ISIDE,2)

	    IF (NSCORE(IUNIT,ISIDE) .GT. 0 ) THEN
	      T2SEARCH(IUNIT,ISIDE) = CLOCK + CTICK
	    ELSE
	      T2SEARCH(IUNIT,ISIDE) = TNEVER
	    ENDIF

	    CALL ALTER_DTEC(IUNIT, ISIDE )
	    CALL DOPLAN
	    CALL GET_NEXT_DTEC (IUNIT, ISIDE, ETIME)
	  ENDDO

c	  TIMESTAMP2  =  SECNDS( 0.0 )
c	  write (IFILELOG,"(F12.3,' Enemy     start ',f15.4,' end ',f15.4)") CLOCK, TIMESTAMP1, TIMESTAMP2

C	**********************************************************

	  IF (DOSEEOWN .GE. 1 ) THEN

D	  IF (DEBUGCYCL .GT. 0 ) THEN
D	    WRITE (KLINE$, "('Checking detection vs friends')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

c	  TIMESTAMP1  =  SECNDS( 0.0 )

	  CALL GET_NEXT_FRIENDS (IUNIT, ISIDE, ETIME)
	  DO WHILE (ETIME .LE. CLOCK .AND. IUNIT .GT. 0)

D	    IF (DEBUGCYCL .GT. 0 ) THEN
D	      WRITE (KLINE$, "(4X,'Icon ',I6,x,I2)") IUNIT, ISIDE
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Time ',F12.3)") ETIME
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Icon time ',F12.3)") T2SEARCH(IUNIT, ISIDE)
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    NONMY = 0
	    IFORCE = MYFORCE(IUNIT,ISIDE)

	    JSIDE = ISIDE

	    IENEMY = 2
	    CALL GET_SEARCH_TIME  ( IFORCE, IENEMY, CTICK )

	    CALL DETECTIONS ( IUNIT, ISIDE, NONMY, CTICK, JSIDE )
	    ISEETRGTS(IUNIT,ISIDE,JSIDE) = NONMY

	    ISEENMY( IUNIT,ISIDE) = ISEETRGTS(IUNIT,ISIDE,1) +
     *		ISEETRGTS(IUNIT,ISIDE,2)

	    IF (NSCORE(IUNIT,ISIDE) .GT. 0 ) THEN
	      T2FRIENDS(IUNIT,ISIDE) = CLOCK + CTICK
	    ELSE
	      T2FRIENDS(IUNIT,ISIDE) = TNEVER
	    ENDIF

	    CALL ALTER_FRIENDS(IUNIT, ISIDE )
	    CALL DOPLAN
	    CALL GET_NEXT_FRIENDS (IUNIT, ISIDE, ETIME)
	  ENDDO

c	  TIMESTAMP2  =  SECNDS( 0.0 )
c	  write (IFILELOG,"(F12.3,' Friends   start ',f15.4,' end ',f15.4)") CLOCK, TIMESTAMP1, TIMESTAMP2

	  ENDIF

C	**********************************************************

D	  IF (DEBUGCYCL .GT. 0 ) THEN
D	    WRITE (KLINE$, "('Checking detection vs obstacles')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

c	  TIMESTAMP1  =  SECNDS( 0.0 )

	  CALL GET_NEXT_DOBS (IUNIT, ISIDE, ETIME)
	  DO WHILE (ETIME .LE. CLOCK .AND. IUNIT .GT. 0)

D	    IF (DEBUGCYCL .GT. 0 ) THEN
D	      WRITE (KLINE$, "(4X,'Icon ',I6,x,I2)") IUNIT, ISIDE
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Time ',F12.3)") ETIME
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Icon time ',F12.3)") T2OBS(IUNIT, ISIDE)
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    CALL WHAT_SENSOR ( IUNIT, ISIDE, ISENS, ISTYP, IBAND )
	    IF (IBAND .LE. 0 ) THEN

D	      IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	        WRITE (KLINE$,"('No valid sensor found. No further processing.')")
D	        CALL TRACEOUT (0,0,0)
D	      ENDIF

	    ELSEIF (IBAND .LT. IBAND_SAR1 ) THEN
	      CALL DETECT_MINES (IUNIT, ISIDE)
	      CALL DETECT_OBSTACLES (IUNIT, ISIDE)
	      CALL DETECT_PITS (IUNIT, ISIDE)
	    ENDIF

	    IF (NSCORE(IUNIT,ISIDE) .GT. 0 ) THEN
	      T2OBS(IUNIT,ISIDE) = CLOCK + DTOBS
	    ELSE
	      T2OBS(IUNIT,ISIDE) = TNEVER
	    ENDIF

	    CALL ALTER_DOBS(IUNIT, ISIDE )
	    CALL GET_NEXT_DOBS (IUNIT, ISIDE, ETIME)
	  ENDDO


c	  TIMESTAMP2  =  SECNDS( 0.0 )
c	  write (IFILELOG,"(F12.3,' Obstacle  start ',f15.4,' end ',f15.4)") CLOCK, TIMESTAMP1, TIMESTAMP2

C	**********************************************************

	  IF (DEBUGCYCL .GT. 0 ) THEN
	    WRITE (KLINE$, "('Checking casualties')")
	    CALL TRACEOUT (0,0,0)
	  ENDIF

	  CALL GET_NEXT_CAS (IUNIT, ISIDE, ETIME)
	  DO WHILE (ETIME .LE. CLOCK .AND. IUNIT .GT. 0)

D	    IF (DEBUGCYCL .GT. 0 ) THEN
D	      WRITE (KLINE$, "(4X,'Icon ',I6,x,I2)") IUNIT, ISIDE
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Time ',F12.3)") ETIME
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Icon time ',F12.3)") T2CAS(IUNIT,ISIDE)
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    CALL CASASSESS (IUNIT, ISIDE)
	    T2CAS(IUNIT,ISIDE) = CLOCK + DSTCAS
	    CALL ALTER_CAS(IUNIT, ISIDE )
	    CALL DOPLAN
	    CALL GET_NEXT_CAS (IUNIT, ISIDE, ETIME)
	  ENDDO

D	  IF (DEBUGCYCL .GT. 0 ) THEN
D	    WRITE (KLINE$, "('Checking direct fire')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

c	  TIMESTAMP1  =  SECNDS( 0.0 )

	  CALL GET_NEXT_DFIR (IUNIT, ISIDE, ETIME)

	  DO WHILE (ETIME .LE. CLOCK .AND. IUNIT .GT. 0)

c	write (IFILELOG,"('Processing icon direct fire ',I4, I4,F12.5,f12.5)") IUNIT, ISIDE, etime, clock

D	    IF (DEBUGCYCL .GT. 0 ) THEN
D	      WRITE (KLINE$, "(4X,'Icon ',I6,x,I2)") IUNIT, ISIDE
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Time ',F12.3)") ETIME
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$, "(4X,'Icon time ',F12.3)") FIRNXT(IUNIT, ISIDE)
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    TNEXT_FIRE = FIRNXT(IUNIT,ISIDE)
	    CALL DFIR( IUNIT,ISIDE, TNEXT_FIRE)
	    FIRNXT(IUNIT,ISIDE) = TNEXT_FIRE
	    CALL ALTER_DFIR(IUNIT, ISIDE )
	    CALL DOPLAN
	    CALL GET_NEXT_DFIR (IUNIT, ISIDE, ETIME)
	  ENDDO

c	  TIMESTAMP2  =  SECNDS( 0.0 )
c	  write (IFILELOG,"(F12.3,' Dfire     start ',f15.4,' end ',f15.4)") CLOCK, TIMESTAMP1, TIMESTAMP2

c	ENDIF


1015	CONTINUE

	TNEXT(1) = TNEXT(1) + SMALLTIME
c	print *,'runjan:next time ',tnext(1)

D	IF (DEBUGCYCL .GT. 0 ) THEN
D	  WRITE (KLINE$, "('Event clock (tnext(1)) set to ',F12.4)") TNEXT(1)
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	GO TO 500

C---------------------- Process "DOCLOUDS"
C---------------------- Update the smoke and dust clouds

 102    CONTINUE
	CALL DOCLOUDS
	GOTO 500

C---------------------- Process STATWT
C---------------------- Write unit positions & status to disk

 103	CONTINUE
	CALL STATWT
	TNEXT(3)  =  TNEXT(3) + PERIOD

D	IF (DEBUGCYCL .GT. 0 ) THEN
D	  WRITE (KLINE$, "('Event clock (tnext(3)) set to ',F12.4)") TNEXT(3)
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	GOTO 500

C---------------------- Suppression recovery

 104	CONTINUE
	DO ISIDE = 1, NUMSIDES
	  DO IUNIT = IBEG(ISIDE,IBRACKET),IEND(ISIDE,IBRACKET)
	    IF ( IUNIT .GT. 0 ) THEN
	      CALL DOPLAN
	      IF ( TSUPRS(IUNIT,ISIDE) .GT. 2.0 ) THEN
	        TSUPRS(IUNIT,ISIDE) = 2.0
	      ENDIF
	      IF ( TSUPRS(IUNIT,ISIDE) .GT. 0.0 ) THEN

D	        IF (DEBUGSUPR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	          CALL ICON_ID( IUNIT, ISIDE, KLINE$)
D	          CALL TRACEOUT (0,2,0)
D	          WRITE (KLINE$,"('Recovery.')")
D	          CALL TRACEOUT (0,0,0)
D	          WRITE (KLINE$,"('Suppression amount ',F6.2,'%')")
D     *			TSUPRS(IUNIT,ISIDE) * 100
D	          CALL TRACEOUT (0,0,0)
D	        ENDIF

	        TSUPRS(IUNIT,ISIDE) = TSUPRS(IUNIT,ISIDE) - SUPRMIN

D	        IF (DEBUGSUPR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	          WRITE (KLINE$,"('Suppression amount ',F6.2,'%')")
D     *		    TSUPRS(IUNIT,ISIDE) * 100
D	          CALL TRACEOUT (0,0,0)
D	        ENDIF

	      ENDIF
	      IF ( TSUPRS(IUNIT,ISIDE) .LT. 0.0 ) THEN
	        TSUPRS(IUNIT,ISIDE) = 0.0
	      ENDIF
	    ENDIF
	  ENDDO
	ENDDO

	TNEXT(4)  =  TNEXT(4) + SUPRMIN
	GOTO 500

C---------------------- Process "INTACT"
C---------------------- clear the graphics que  .. display firings, smoke..etc.

 105	CONTINUE
	IRVIEW = MOD(IRVIEW,NUMSTAT) + 1
	IF( DSPPHY(IRVIEW) .GE. 1 )  THEN
	  CALL INTACT ( IRVIEW )
	ENDIF

	TNEXT(5)  =  CLOCK +  (GUPINT / NUMSTAT)
	GO TO 500

  106	CONTINUE
	! cntrbat no longer used
	TNEXT(6)  =  TNEVER
	GOTO 500

C---------------------- Process "SEARCH"
C---------------------- Update target acquisitions

107	CONTINUE
1071	TNEXT(7)  = TNEVER
	GOTO 500

C---------------------- Process 'RADTIM'
C---------------------- Transition units to different radiation states.

 108	CONTINUE
	CALL DOCHEM
	GOTO 500

C---------------------- Process "FIRING"
C---------------------- Update the results of a weapon firing.

 109	CONTINUE
	CALL FIRING
	GOTO 500

C---------------------- Process "IMPACT"
C---------------------- update the results of a round impacting.

 110	CONTINUE
	CALL IMPACT
	GOTO 500

C---------------------- Process "RADAR"
C---------------------- update an air defense radar state.

 111	CONTINUE

C	CALL RADAR
C	CALL CHECK_CBR

	CALL ACTIVE_SENSORS

	TNEXT(11) = CLOCK + DSTRADAR
	GOTO 500

C---------------------- Process "COPTER"
C---------------------- update a helicopter state.

 112	CONTINUE
C	CALL COPTER
	GOTO 500

C---------------------- Process "DOARTY"
C---------------------- Startup Indirect Fire Missions

 113	CONTINUE
	CALL DOARTY
	TNEXT(13)  =  CLOCK + 0.05			! DT = 3 sec

  500	CONTINUE

C ---- Check if status reports need to be sent (THN 7-MAY-2001)
	IF( CLOCK .GE. NET_STATFREQ ) THEN
	   CALL WTSTATUS
	   NET_STATFREQ = NET_STATFREQ + NET_STATUSRPT
	ENDIF

C------ Perform automatic checkpoint save, if time to do so

	IF( CLOCK .GE. CKPTTIM )  THEN
	    CALL DOCKPT
	  IF( CKPTFREQ .GT. 0.0 )  THEN
	    CKPTTIM  =  CLOCK + CKPTFREQ
	  ELSE
	    CKPTTIM = TNEVER
	  ENDIF

	ENDIF

	IF ( CLOCK .GT. END_TIME ) THEN
	  KRESET = 2	! end the game
	ENDIF

600	CONTINUE

	ENDDO

C------ RETURN to main driver

	IRESET  =  KRESET
	IF( KRESET .EQ. 3 )  KKD = 1
	IF( KRESET .EQ. 4 )  KKD = 2

	RETURN
	END


	SUBROUTINE BRACKETS ()

	include 'global.f'
	include 'globunits.f'
	include 'globmove.f'
	include 'globsetup.f'
	include 'globrpt.f'

	PARAMETER	MAXBRACKETS = 5
	INTEGER*4	IBEG(NUMSIDES,MAXBRACKETS),
     *			IEND(NUMSIDES,MAXBRACKETS),
     *			IBRACKET

	INTEGER*4	IUNIT / 1 /
	INTEGER*4	ISIDE / 1 /

	INTEGER*4	NUMBRACKETS(NUMSIDES)

	ENTRY INIT_BRACKETS()

	DO I=1, NUMSIDES
	  NUMBRACKETS(I) = MAXBRACKETS
	  IF ( KNUMUNITS(I) .GT. MAXBRACKETS ) THEN
	    ICNT = KNUMUNITS(I) / MAXBRACKETS
	    I1 = 1
	    DO J=1,MAXBRACKETS
	      IBEG(I,J) = I1
	      I1 = I1 + ICNT
	      IEND(I,J) = I1 - 1
	    ENDDO
	    IEND(I,MAXBRACKETS) = KNUMUNITS(I)
	  ELSE
	    NUMBRACKETS(I) = 1
	    DO J=1,MAXBRACKETS
	      IBEG(I,J) = 0
	      IEND(I,J) = 0
	    ENDDO
	    IBEG(I,1) = 1
	    IEND(I,1) = KNUMUNITS(I)
	  ENDIF
	ENDDO

	IBRACKET = 1
	IPOINTER = 1
	ISIDE = 1

	RETURN

	ENTRY GET_NEXT_ICON ( IU, IS, ISTOP)

	ISTOP = 0

D	IF (DEBUGCYCL .GT. 0 ) THEN
D	  WRITE (KLINE$, "('Bracket ',I2)") IBRACKET
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$, "('ISIDE ',I5)") ISIDE
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IU = IPOINTER
	IS = ISIDE

100	continue

	IPOINTER = IPOINTER + 1
	IF ( IPOINTER .GT. IEND(ISIDE,IBRACKET)) THEN
	  ISTOP = 1
	  ISIDE = ISIDE + 1
	  IF ( ISIDE .GT. NUMSIDES) THEN
	    ISIDE = 1
	    IBRACKET = IBRACKET + 1
	    IF ( IBRACKET .GT. NUMBRACKETS(ISIDE)) THEN
	      IBRACKET = 1
	    ENDIF
	  ENDIF
	  IPOINTER = IBEG(ISIDE,IBRACKET)
	  IF (POINTER .LE. 0 ) THEN
	    GOTO 100
	  ENDIF
	ENDIF

	RETURN
	END



	 */

}
