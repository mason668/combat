package sim;

import models.Event;
import models.EventQueue;
import utils.Logger;
import utils.Parser;

public class Janus {
	
	private String scenarioName = null;
	private Scenario scenario = null;
	private EventQueue eventQueue = new EventQueue();
	private boolean running = true;
	private boolean clockPaused = false;
	private double timeZero;
	private double timeLast;

	public static void main(String[] args){
		Janus janus = new Janus();
		janus.test(args);
	}
	private void doArgs(String[] args){
	}
	public void test(String[] args){
		doArgs(args);
		run();
	}

	public void run(){
		initData();
		boolean restart = true;
//50
		while (restart){
			restart = false;
			int kreset = 0;// game has not yet ended?
			restart = jansetup();
			janIgrafax();
	//100
			initialiseSocket();
			kreset = initmain();
			if (kreset <= 0){
				kreset = runjan(kreset);
			}
//			IF( KRESET .GE. 3 )      GO TO 100
			closeSocket();
			endjan();
		}
		//stop
	}
	
	private void initData(){ //TODO
		Logger.log("initialising data");
	}
	private boolean jansetup(){ //TODO
		Logger.log("setting up scenario");
		scenario = new Scenario(scenarioName);
		return false;
	}
	private void janIgrafax(){
		Logger.log("initialising graphics");
	}
	private int initmain(){
		Logger.log("initial planning");
		return 0;
	}
	private void initialiseSocket(){} //TODO initialise socket
	private void closeSocket(){} //TODO close socket
	private void endjan(){
		Logger.log("ending scenario");
	}
	private int runjan(int i){
		Logger.log("running sim");
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
	*/
		double checkPointTime = Constants.NEVER;
		if (scenario.getParameters().getCheckPointFrequency()>0){
			checkPointTime = scenario.getClock() + 
					scenario.getParameters().getCheckPointFrequency();
		}
		
		/*
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

*/
		for (int j=0;j<10;j++){
			Event e = new Event();
			e.setTime(2.0 + Math.random()* 10.0);
			eventQueue.add(e);
		}
		double clock = scenario.getParameters().getStartTime();
		timeZero = System.currentTimeMillis();
		timeLast = clock*1000;

		while (running){
			//doplan
			if (! clockPaused){
				doEvents();
			}
			
		}
		/*
		 * 
		DO WHILE  ( KRESET .EQ. 0 )

		CALL DOPLAN

		IF ( CLOCK_PAUSED ) THEN
		  GOTO 600
		ENDIF

	600	CONTINUE

		ENDDO

	C------ RETURN to main driver

		IRESET  =  KRESET
		IF( KRESET .EQ. 3 )  KKD = 1
		IF( KRESET .EQ. 4 )  KKD = 2

		RETURN
		END
		 */
		return 0;

	}
	
	private void doEvents(){
		double nextTime = eventQueue.nextTime();
		//TODO set scenario clock
		Logger.say("event " + nextTime);
		if (nextTime >= Constants.NEVER){
			running = false;
			return;
		}
		eventQueue.doEvents(nextTime);
		if (scenario.getParameters().getRealTimeSynch()){
			double gameClock = nextTime * 1000;
			double gameDelta = gameClock - timeLast;
			double wallDelta = System.currentTimeMillis() - timeZero;
			wallDelta = wallDelta * scenario.getParameters().getRealTimeRatio();
			//Logger.say("times a " + gameDelta + " : " + wallDelta);
			double diff = gameDelta - wallDelta;
			//TODO don't use dif, but a small figure
			// TODO call doplan instead
			while (diff > 0){
				try {
					Thread.sleep((int) diff);
				} catch (Exception exception){}
				wallDelta = System.currentTimeMillis() - timeZero;
				wallDelta = wallDelta * scenario.getParameters().getRealTimeRatio();
				diff = gameDelta - wallDelta;
			}
			//Logger.say("times b " + gameDelta + " : " + wallDelta + " : " + nextTime);
			Logger.say("clock : " + Parser.formatTime(nextTime));

			if ( gameDelta > 2000){
				timeZero = System.currentTimeMillis();
				timeLast = gameClock;
			}
		}
		/*
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


		 * 
		 */
		
		/*
		IF ( CLOCK .GT. END_TIME ) THEN
		  KRESET = 2	! end the game
		ENDIF
		 * 
		 */
		if (nextTime >= scenario.getParameters().getEndTime()){
			running = false;
		}
		return;
	}
}
