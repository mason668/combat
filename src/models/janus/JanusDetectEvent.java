package models.janus;

import java.util.Iterator;
import java.util.Vector;

import data.csd.Sensor;
import models.DetectEvent;
import models.Event;
import sim.Constants;
import sim.GameClock;
import sim.Scenario;
import sim.Simulation;
import sim.entity.DetectedEntity;
import sim.entity.Entity;
import sim.entity.ObserverEntity;
import utils.Logger;
import utils.Parser;
import utils.Tracer;

public class JanusDetectEvent extends DetectEvent{

	private Scenario myScenario;
	private ObserverEntity myEntity;
	private GameClock gameClock;

	public JanusDetectEvent(double time, Entity entity, Simulation sim) {
		super(time, entity, sim);
		myScenario = sim.getScenario();
		myEntity = entity;
		gameClock = sim.getGameClock();
	}

	public Event doEvent(){
		tracing = this.myEntity.isTracing();		
		if (tracing){
			Tracer.write(Tracer.DETECTION,0,"\nprocessing detection ");
			Tracer.write(Tracer.DETECTION,0,"for entity " + myEntity.getID());
			Tracer.write(Tracer.DETECTION,0,"event time: " + 
					Parser.formatTime(this.getTime()) + "  " + this.getTime());
			Tracer.write(Tracer.DETECTION,0,"game clock: " + 
					Parser.formatTime(gameClock.getClockSecs()) + 
					"  " + gameClock.getClockSecs());
		}
		if (this.myEntity.getForce() == null){
			if (tracing){
				Tracer.write(Tracer.DETECTION,0,"observer has no valid force, unable to continue "); 
			}
			Logger.err(Logger.ERROR, "entity " + this.myEntity.getID() + " has invalid force");
			return null;
		}
		if (tracing){
			Tracer.write(Tracer.DETECTION,0,"force: " + myEntity.getForce().getName());
			Tracer.write(Tracer.DETECTION,0,"detection cycle time: " + 
					Parser.formatTime(this.myEntity.getForce().getScanTime()) + 
					"  " + this.myEntity.getForce().getSearchTime());
		}
		detect();
		if (!this.myEntity.isDead()){
			this.setTime(this.getTime() + this.myEntity.getForce().getSearchTime());
			return this;
		}
		return null;
	}
	
	private void detect(){
		Sensor sensor = this.myEntity.getCurrentSensor();
		if (tracing){
			if (sensor == null){
				Tracer.write(Tracer.DETECTION,1,"no active sensor ");
			} else {
				Tracer.write(Tracer.DETECTION,1,"using sensor " + sensor.getName());
			}
		}
		// validate sensor band
		if (sensor.getBand()<0 || sensor.getBand()> Sensor.MAX_SENSOR_BANDS){
			if (tracing){
				Tracer.write(Tracer.DETECTION,1,"invalid sensor band " + sensor.getBand());
			}
			//TODO should probably reset actual sensor
			sensor = null;
		} else {
			if (tracing){
				Tracer.write(Tracer.DETECTION,1,"sensor band " + sensor.getBand() + " " +
						Sensor.SENSOR_BANDS[sensor.getBand()]);
			}
		}
		
		changeSensor (sensor);
		
		if ( sensor.getBand()== Sensor.BAND_MMR){
			detectSAR();
		} else {
			detectOpticalThernal(sensor);
		}

		this.myEntity.setLastSensor(sensor);
	}
	
	private void changeSensor(Sensor sensor){
		Sensor lastSensor = this.myEntity.getLastSensor();
		if (lastSensor == sensor) {
			if (tracing){
				Tracer.write(Tracer.DETECTION,1,"using the same sensor as last event");
			}
			return;
		}
		if (tracing){
			Tracer.write(Tracer.DETECTION,1,"changed sensor since last event");
		}
		if (lastSensor == null){
			if (tracing){
				Tracer.write(Tracer.DETECTION,1,"previous sensor was null");
			}
			return;
		}
		if (tracing){
			Tracer.write(Tracer.DETECTION,1,"previous sensor was " + lastSensor.getName());
		}
		if (lastSensor.getBand()==Sensor.BAND_MMR){
			if (tracing){
				Tracer.write(Tracer.DETECTION,1,"previous sensor was MMR");
			}
			//TODO MMR sensor
			/*
D	      IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	        WRITE(KLINE$,"('Last sensor was an mmr sensor')")
D	        CALL TRACEOUT (0,0,0)
D	        WRITE(KLINE$,"('Removing targets detected by icon')")
D	        CALL TRACEOUT (0,0,0)
D	        WRITE(KLINE$,"('Removing false MMR targets')")
D	        CALL TRACEOUT (0,0,0)
D	      ENDIF

	      CALL CLEAR_DETECTED_TARGETS(IUNIT,ISIDE,IVIEW,JSIDE,NONMY,LSENS)
	      CALL CLEAR_DETECTED_TARGETS(IUNIT,ISIDE,IVIEW,3 - JSIDE,NONMY,LSENS)
	      ISAR = ISARNUMBER(IUNIT, ISIDE, LAST_SENSOR(IUNIT,ISIDE))
	      CALL CLEAR_FALSE_TARGETS(IUNIT,ISIDE,ISAR,IVIEW)
			 * 
			 */
			return;
		}
		if (sensor == null){
			if (tracing){
				Tracer.write(Tracer.DETECTION,1,"new sensor null, clearing all detections");
			}
//			clearDetectedTargets();
			return;
		}
		if (sensor.getBand()== Sensor.BAND_MMR){
			if (tracing){
				Tracer.write(Tracer.DETECTION,1,"new sensor is an MMR");
			}
			/*
			 * TODO MMR sensor

D	      IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	        WRITE(KLINE$,"('Switched to an mmr sensor')")
D	        CALL TRACEOUT (0,0,0)
D	        WRITE(KLINE$,"('Removing targets detected by icon')")
D	        CALL TRACEOUT (0,0,0)
D	      ENDIF

	      CALL CLEAR_DETECTED_TARGETS(IUNIT,ISIDE,IVIEW,JSIDE,NONMY,LSENS)
	      CALL CLEAR_DETECTED_TARGETS(IUNIT,ISIDE,IVIEW,3 - JSIDE,NONMY,LSENS)
			 * 
			 */
			return;
		}
		if (tracing){
			Tracer.write(Tracer.DETECTION,1,"new sensor is a normal sensor");
			Tracer.write(Tracer.DETECTION,1,"will hand off old targets to new sensor");
		}
	}
	
	private void detectOpticalThernal(Sensor sensor){ // findnmy
		if (tracing){
			Tracer.write(Tracer.DETECTION,1,"using optical/ thermal sensor model");
		}

		/*
	SUBROUTINE  FINDNMY ( IUNIT, ISIDE, NONMY, CTICK, JSIDE, ISNSR, ISENS )

	IFORCE = MYFORCE(IUNIT,ISIDE)
	ISCORE = NSCORE(IUNIT,ISIDE)

	IENEMY = 1
	IF ( ISIDE .EQ. JSIDE) IENEMY = 2

	MAX_DETECTIONS = FORCE_DETECTIONS(IFORCE,IENEMY)
D	IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	  WRITE (KLINE$,"('MODULE: FINDNMY.F')")
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF ( IFORCE .LT. 1 ) THEN
	  TYPE *,'FINDNMY - INVALID FORCE',IFORCE,IUNIT,ISIDE
	  GOTO 999
	ENDIF

	*/
		Vector<Entity> canSee = new Vector<Entity>();
	/*

	IF (ISENS .EQ. 0 ) GOTO 999

C	IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
C	  WRITE (KLINE$,"('Sensor Name ',A16)") SNSNAME$(ISENS)
C	  CALL TRACEOUT (0,0,0)
C	ENDIF

	*/
		if (tracing){
			Tracer.write(Tracer.DETECTION,2,"determining observer status");
		}
		
		if (cantDetect()){
			if (tracing){
				Tracer.write(Tracer.DETECTION,2,"unable to perform detection process");
			}
			//TODO should return or something
		} // else TODO
		
		double probd = calculateProbabilityModifier(sensor);
	/*

	IBUILD = IN_BUILDING(IUNIT,ISIDE)
	IWALL = AT_WALL(IUNIT,ISIDE)
	IFLOOR = ON_FLOOR(IUNIT,ISIDE)
	IROOF = ON_ROOF(IUNIT,ISIDE)

	CALL UNITXY ( IUNIT,ISIDE, XO,YO )

	CALL UNIT_SENSOR_HEIGHT_AGL(IUNIT, ISIDE, ZO)

	DIR1	=  DVIEW(IUNIT,ISIDE)

	IF ( DIR1 .LT. 0 ) THEN
	  DIR1 = DIR1 + TWOPI
	ENDIF

	AL	=  DLEFT(IUNIT,ISIDE)

	CALL UNIRAN (DRAW)
	IF ( DRAW .LT. SECONDARY_FOV_RATIO)  THEN
	ELSE
	  AL = MIN (AL, FOVPRIMARY(ITYPE))
	ENDIF

	AR      =  ONEOVERPI * AL
	AL2 = AL*2

D	IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	  WRITE (KLINE$, "(4X,'Observer location (utm) = ',F12.3,XX,F12.3,XX,F12.3)") XO, YO, ZO
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$, "(4x,'Observer is on roof of building ',I6)") IROOF
D	  CALL TRACEOUT(0,0,0)
D	  WRITE (KLINE$, "(4x,'Observer is inside building ',I6)") IBUILD
D	  CALL TRACEOUT(0,0,0)
D	  WRITE (KLINE$, "(4x,'On floor ',I6)") IFLOOR
D	  CALL TRACEOUT(0,0,0)
D	  WRITE (KLINE$, "(4x,'And at wall ',I6)") IWALL
D	  CALL TRACEOUT(0,0,0)
D	  WRITE (KLINE$, "(4X'Observer mast height above ground ',F8.1,'m')") ZO
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$, "(4X,'Direction of view = ',F8.4,' radians or ',F8.4,' degrees')") DIR1, DIR1*C2DEG
D	  CALL TRACEOUT(0,0,0)
D	  WRITE (KLINE$, "(4X,'Field of view =     ',F8.4,' radians or ',F8.4,' degrees')") AL2, AL2*C2DEG
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	*/
		double vislim = myEntity.getPlatform().getMaxVisibility();
		if (sensor == null) {
			vislim = 0.0;
		} else {
			vislim = Math.min(vislim, sensor.getMaxVisibility());
		}
		double visSqr = vislim * vislim;
		double xmin = myEntity.getLocation().getX()-vislim;
		double xmax = myEntity.getLocation().getX()+vislim;
		double ymin = myEntity.getLocation().getY()-vislim;
		double ymax = myEntity.getLocation().getY()+vislim;
		
		if (tracing){
			Tracer.write(Tracer.DETECTION,3,"maximum visibility " + vislim);
			Tracer.write(Tracer.DETECTION,3,"vis squared " + visSqr);
			Tracer.write(Tracer.DETECTION,3,"xmin " + xmin);
			Tracer.write(Tracer.DETECTION,3,"xmax " + xmax);
			Tracer.write(Tracer.DETECTION,3,"ymin " + ymin);
			Tracer.write(Tracer.DETECTION,3,"ymax " + ymax);
		}
		
		double dview = myEntity.getDirectionView();
		double fov = Math.PI * 0.5; // TODO use actual fov
		
		double probfov = 1.0;
		if ( fov <= 0.0) {
			probfov = 0.0;
		} else {
			if (sensor.getFov()> fov){
				probfov = sensor.getFov() / fov; // TODO what about ctick?
			} else {
				probfov = 1.0;
			}
		}
		probd = probd * probfov;

	/*
C------ Fetch observer's view-fan parameters

	ITICK = CTICK * 60.0

	IF ( AL2 .NE. 0 ) THEN
	  IF ( SENSFOV(ISENS) .LT. AL2) THEN
	    PROBFOV = (ITICK * SENSFOV(ISENS)) / (AL2)
	    PROBFOV = MIN(1.0,PROBFOV)
	  ELSE
	    PROBFOV = 1.0
	  ENDIF
	  PROBD = PROBD * PROBFOV
	ENDIF
	
	*/
		if (tracing){
		Tracer.write(Tracer.DETECTION,3,"fov " + fov); //TODO more traces
		}
	/*


D	IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) ) THEN
D	  WRITE (KLINE$,"(4X,'Sensor   FOV', F12.3,' radians or ',F12.3,' degrees')") SENSFOV(ISENS), SENSFOV(ISENS)*C2DEG
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$, "(4x,'Detection cycle time ',F12.3,' secs')") CTICK*60
D	  CALL TRACEOUT(0,0,0)
D	  WRITE (KLINE$,"(4X,'Ratio of Observer FOV to sensor = ', F12.3)") PROBFOV
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(4X,'PD multiplier = ', F12.3)") PROBD
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	TEFFTIM = CTICK

	! set distance threshold for observer footprint
	IF ( KNUMELE(IUNIT,ISIDE) .GT. 1 ) THEN
	  RNGPOS = NSCORE(IUNIT,ISIDE) * NMIN(ITYPE)
	  RNGPOS = RNGPOS * 0.001 * 0.222
	ELSE
	  RNGPOS = 0.005
	ENDIF

	*/
		
		if (tracing){
			Tracer.write(Tracer.DETECTION,2,"final PD multiplier =  " +
					probd);
			Tracer.write(Tracer.DETECTION,1,"now searching through target list");
		}

		detectTargets(sensor, xmin, xmax, ymin, ymax, visSqr, dview, fov);

		if (tracing){
			Tracer.write(Tracer.DETECTION,1,"total number of targets seen " +
					canSee.size());
		}
	}
	
	private void detectSAR(){
		//TODO MMR
		/*
	  IF (MOUNTED(IUNIT,ISIDE) .GT. 0 ) THEN

D	    IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	      WRITE (KLINE$,"('MMR Sensors do not work while mounted.')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	  ELSE
	    ISAR_TYPE = ISTYP - NUMSENSTYPS

D	    IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	      WRITE (KLINE$,"('Using MMR detection model for sar number ',I3.3)") ISAR_TYPE
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    ISAR = ISARNUMBER(IUNIT, ISIDE, ISENS) ! get the sar instance number

	    IF (ISENS .NE. LAST_SENSOR(IUNIT,ISIDE)) THEN
	      SAR_LIST(ISAR).RESCAN = .TRUE.
	    ENDIF

             IF (SAR_LIST(ISAR).GMTI .EQ. .TRUE.) THEN
               GMTI_MSG$ = 'ON'
             ELSE
               GMTI_MSG$ = 'OFF'
             ENDIF

D	    IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D             IF (SAR_LIST(ISAR).SAR .EQ. .TRUE.) THEN
D	        SAR_MSG$ = 'ON'
D              ELSE
D                SAR_MSG$ = 'OFF'
D              ENDIF
D              WRITE (KLINE$,"('SAR is ',A3,' |    GMTI is ',A3)	") SAR_MSG$, GMTI_MSG$
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    IF ( SAR_LIST(ISAR).FOOTPRINT_MODE .EQ. STRIPMAP ) THEN
	      CALL STRIPMAP_DETECT ( IUNIT, ISIDE, ISTYP, ISENS,
     *	 		             NONMY, JSIDE )
            ENDIF
            IF ( SAR_LIST(ISAR).FOOTPRINT_MODE .EQ. SPOTLIGHT ) THEN
              CALL SPOTLIGHT_DETECT ( IUNIT, ISIDE, ISTYP, ISENS,
     *                                NONMY, JSIDE )
	    ENDIF
	    IF (ISIDE .NE. JSIDE) THEN
              CALL SEARCH_FALSE_TARGETS(IUNIT,ISIDE,ISAR)
	    ENDIF
	  ENDIF ! mounted
c	  CTICK = DTSEARCH

		 * 
		 */
		
	}
	
	private boolean cantDetect(){
		return false; // TODO
		/*
C--------- Is it dead ?
	INUM = NSCORE(IUNIT,ISIDE)

D	IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	  WRITE (KLINE$,"(4X,'Number of elements remaining ',I3)") INUM
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF( INUM .LT. 1 )     THEN
	  IAMDEAD = 1
	  GOTO 400
	ENDIF

C--------- Is it chemed ?
        IF( KINOPSTAT(IUNIT,ISIDE) .NE. 0 )   THEN
	  IAMDEAD = 1
	  GOTO 400
	ENDIF

C--------- Is it mounted ?
	IF( MOUNTED(IUNIT,ISIDE) .GT. 0 )  THEN
	  IHOST  =  MOUNTED(IUNIT,ISIDE)
	  IHTYP  =  KSYSTYP(IHOST,ISIDE)

D	  IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	    WRITE (KLINE$,"(4X,'Observer is mounted in icon ',I6)") IHOST
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(8X,'Host flag is set to ',I2)") KANHOST(IHTYP,ISIDE)
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IF( KANHOST(IHTYP) .LT. 2 )  THEN

D	    IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	      WRITE (KLINE$,"(8X,'Cant acquire while mounted')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    IAMDEAD = 1
	    GOTO 400
	  ENDIF
	ENDIF

		 * 
		 */
	}
	
	private double calculateProbabilityModifier(Sensor sensor){
		double probd = 1.0;
		if (tracing){
			Tracer.write(Tracer.DETECTION,3,"current suppression level " + myEntity.getSuppressionAmount());
		}
		if ( myEntity.isSuppressed()){
			probd = myEntity.getPlatform().getSuppressionFactorPD();
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"observer is suppressed");
				Tracer.write(Tracer.DETECTION,3,"PD = " + probd);
			}
		} else{
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"observer is not suppressed");
				Tracer.write(Tracer.DETECTION,3,"PD = " + probd);
			}
		}
		if (myEntity.iAmRunning()){
			probd = probd * myEntity.getPlatform().getRunFactorPD();
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"observer is running");
				Tracer.write(Tracer.DETECTION,3,"PD = " + probd);
			}
		}
		if (myEntity.iAmCrawling()){
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"observer is crawling");
				Tracer.write(Tracer.DETECTION,3,"PD = " + probd);
			}
		} else if (myEntity.getCurrentSpeed()>0.0 ){
			probd = probd * myEntity.getPlatform().getMoveFactorPD();
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"observer is moving");
				Tracer.write(Tracer.DETECTION,3,"PD = " + probd);
			}
		} else {
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"observer is not moving");
				Tracer.write(Tracer.DETECTION,3,"PD = " + probd);
			}
		}
		
		if (sensor.getBand() == Sensor.BAND_OPTICAL1 || 
				sensor.getBand() == Sensor.BAND_THERMAL1)
		{
			probd = probd * myScenario.getParameters().getWeatherDetect();
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"sensor is subject to day/night");
				Tracer.write(Tracer.DETECTION,3,"PD = " + probd);
			}
		}
		return probd;
	}
	
	private void detectTargets(Sensor sensor, 
			double xmin, double xmax, 
			double ymin, double ymax,
			double visSqr, double dview, double fov){
		Iterator<DetectedEntity> iterator = this.myEntity.getTargetList().iterator();
		while (iterator.hasNext()){
			DetectedEntity target = iterator.next();
			if (tracing){
				Tracer.write(Tracer.DETECTION,2,"attempting to detect " +
						target.getName() + " " + 
						target.getPlatform().getName());
			}
			detectTarget(target, sensor, xmin, xmax, ymin, ymax, visSqr, dview, fov);
		}
	}
	
	private void detectTarget(DetectedEntity target, Sensor sensor,
			double xmin, double xmax, double ymin, double ymax, double visSqr, double dview, double fov){
		int iseehim = Constants.OBSERVATION_LEVEL_UNSEEN;
		boolean doIt = true;
		if ( myEntity.isDead()) { // TODO double check - this is iamdead from findnmy
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"observer is unable to detect targets ");
			}
			doIt = false;
		} 
		if (myEntity.getForce() == target.getForce()){
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"observer and taregt in same force ");
			}
			doIt = false;
		}
		int isawhim = myEntity.getDetectionLevel(target);
		if (tracing){
			Tracer.write(Tracer.DETECTION,3,"previous observation level " + isawhim);
		}
		/*
		 * TODO deal with max targets
	  IF (NONMY .GE. MAX_DETECTIONS ) THEN ! if we have filled target list then
				! we cant detect any extra units
D	    IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	      WRITE (KLINE$,"('Exceeded max # of detections for this observer. (',I3.3,')')") MAX_DETECTIONS
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF
	    GOTO 500
	  ELSE

		 */

		if (doIt){
			iseehim = findUnit(target, sensor, xmin, xmax, ymin, ymax, visSqr, dview, fov);
		}
		updateTargetList(target, iseehim);
		//  NMYLEVEL(IUNIT,ISIDE,J,JSIDE) = ISEEHIM
	}
	
	private int findUnit(DetectedEntity target, Sensor sensor,
			double xmin, double xmax, double ymin, double ymax, double visSqr, double dview, double fov){
		int iseehim = Constants.OBSERVATION_LEVEL_UNSEEN;
		/*
	ISAWHIM = ISEEHIM	! save flag to say he was seen last go

	ISEEHIM = IOBSNONE
	JTYPE  =  KSYSTYP(JUNIT,JSIDE)

	IF (ISIDE.EQ.JSIDE .AND. IUNIT.EQ.JUNIT ) GOTO 600
	
	*/
		if (targetIsDead(target)) {
			return Constants.OBSERVATION_LEVEL_UNSEEN;
		}
		if (observerIsDead()){
			return Constants.OBSERVATION_LEVEL_UNSEEN;
		}
		
		if (!sensorOK(sensor, target)){
			return Constants.OBSERVATION_LEVEL_UNSEEN;
		}
		if (!withinFOV(xmin, xmax, ymin, ymax, visSqr, dview, fov, target)){
			return Constants.OBSERVATION_LEVEL_UNSEEN;
		}
		
		int jconclas = 1; // getjconclas - look in getsize
		double size = getSize();
		
	/*

C-------- get its size

	IF ( JCONCLAS .EQ. 0 ) THEN
	  PRINT *
	  PRINT *,'WARNING: findunit'
	  PRINT *,'Icon ',JUNIT,' on side ',JSIDE
	  PRINT *,'does not have valid thermal contrast data.'
	  PRINT *,'Detection event aborted.'
	  PRINT *
	  GOTO 600
	ENDIF

C-------- can I detect it if I have LOS

	RANGE     =  SQRT( RANGESQR )

	IDEBUG = 0
D	IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	  IDEBUG = 1
D	ENDIF

c	TIMESTAMP1  =  SECNDS( 0.0 )

	CALL PAIRS ( ISENS, JCONCLAS, RANGE, SIZE, 0.0, BARS, IDEBUG )
	CALL GET_ACQ_THRESHOLD (IUNIT, ISIDE, JUNIT, JSIDE, IVAL)

c	TIMESTAMP2  =  SECNDS( 0.0 )
c	write (IFILELOG,"(F12.3,' Pairs     start ',f15.4,' end ',f15.4)") CLOCK, TIMESTAMP1, TIMESTAMP2

	VALUE = PAIRSVAL(IVAL)

D	IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	  WRITE (KLINE$,"(4X,'Initial BARS ',F12.4)") BARS
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(4X,'Threshold for target ',F12.4)") VALUE
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF( BARS .LT. VALUE ) THEN
	  GOTO 600
	ENDIF

C NOTE	The reason that these two routines are done in what might seem a reverse
C NOTE	order is because the LOS calculation is fairly time consuming. It is
C NOTE	best to exclude all other reasons why detection will fail before
C NOTE	performing that calculation.

*/
		if (!withinLOS(target)){
			return Constants.OBSERVATION_LEVEL_UNSEEN;
		}
	/*


C	  IF (RANGE.LE.0.05) THEN  ! if within 50m and los exists
C	    PD = 1.0
C	    GOTO 560
C	  ENDIF

C-------- does smoke block the path

	CALL DOLASLOS ( XO,YO,ZO, XT,YT,ZT, ISNSRCLAS, OLEN )

	IF( OLEN .GT. OLENMAX(ISNSRCLAS) )  THEN
D	  IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	    WRITE (KLINE$,"(4X,'LA Smoke blocks LOS')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF
	  GOTO 600
	ENDIF

	IF( OLEN .GT. 0.0 )  THEN
D	  IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	    WRITE (KLINE$,"(4X,'LA smoke needs to be considered')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF
c	  SIZE  =  SIZE * PLOS
	  CALL PAIRS ( ISENS, JCONCLAS, RANGE, SIZE, OLEN, BARS, IDEBUG )
	ELSE
c	  SIZE  =  SIZE * PLOS
c	  CALL PAIRS ( ISENS, JCONCLAS, RANGE, SIZE, 0.0, BARS )
c	  BARS  =  BARS * PLOS
	ENDIF

D	IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	  WRITE (KLINE$,"(4X,'Final BARS ',F12.4)") BARS
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(4X,'Threshold for target ',F12.4)") VALUE
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF( BARS .GE. VALUE )  THEN
c	  TIMESTAMP1  =  SECNDS( 0.0 )

	  CALL PAIRS ( ISENS, JCONCLAS, RANGE, REALSIZE, OLEN, REALBARS, IDEBUG )

c	  TIMESTAMP2  =  SECNDS( 0.0 )
c	  write (IFILELOG,"(F12.3,' Pairs2    start ',f15.4,' end ',f15.4)") CLOCK, TIMESTAMP1, TIMESTAMP2

D	  IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	    WRITE (KLINE$,"(8X,'Real BARS ',F12.4)") REALBARS
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

c	  TIMESTAMP1  =  SECNDS( 0.0 )

	  CALL CALCPD (IUNIT, ISIDE, JUNIT, JSIDE, JTYPE,
     *		BARS, REALBARS, VALUE, PLOS, PROBD, RANGE, RNGPOS, ISENS, ISNSRCLAS,
     *		ISAWHIM, ISEEHIM )

	ENDIF

		 * 
		 */
		iseehim = limitObservationLevel(iseehim, sensor);
		return iseehim;
	}
	
	private int limitObservationLevel( int level, Sensor sensor){
		/*
	IF ( ISEEHIM .GT. MAXDETLVL(ISENS) ) THEN
	  ISEEHIM = MAXDETLVL(ISENS)
D	  IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	    WRITE (KLINE$, 2001) ISEEHIM
D	    CALL TRACEOUT (0,0,0)
2001	    FORMAT (4X,'Observation level limited to ',I2.2,
     *		' by sensor data')
D	  ENDIF
	ENDIF

		 * 
		 */
		return level;
	}
	
	private boolean targetIsDead(DetectedEntity target){
		if (!target.isDead()){
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"target is not dead");
			}
			return false;
		}

		if (tracing){
			Tracer.write(Tracer.DETECTION,3,"target is dead");
		}

		if ( !myScenario.getParameters().getDoSeeDead()){
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"detection of dead entities is disabled");
			}
			return true;
		}

		if (target.getPlatform().getMoverType() == Constants.MOVER_FOOT){
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"target is a foot mover");
			}
			return true;
		}
		if (tracing){
			Tracer.write(Tracer.DETECTION,3,"target is not a foot mover");
		}
		return false;
	}
	
	private boolean observerIsDead(){
		if (myEntity.isDead()) {
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"observer is dead");
			}
			return true;
		}
		return false;
	}
	
	private boolean targetIsMounted(DetectedEntity target){
		if (target.isMounted()){
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"target is mounted");
			}
			return true;
		}
		return false;
	}
	
	private boolean sensorOK(Sensor sensor, DetectedEntity target){
		if (!sensor.onlyDetectMovers()) return true;
		if (tracing){
			Tracer.write(Tracer.DETECTION,3,"sensor can only detect moving targets");
			Tracer.write(Tracer.DETECTION,3,"target travelling at " + 
					target.getCurrentSpeed()+"kph");
			Tracer.write(Tracer.DETECTION,3,"sensor min speed " + sensor.getMinSpeed());
		}
		if (target.getCurrentSpeed() >= sensor.getMinSpeed()){
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"target travelling fast enough "); 
			}
			return true;
		}
		if (tracing){
			Tracer.write(Tracer.DETECTION,3,"sensor can only detect moving targets");
			Tracer.write(Tracer.DETECTION,3,"target travelling too slow "); 
		}
		return false;
		/* TODO aircraft
		 * 
	JFLYTYPE  =  FLYERS(JTYPE)
	IF ( JFLYTYPE .GT. 0 ) THEN ! target is a flyer

D	  IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	    WRITE (KLINE$,"(4X,'Target is an aircraft')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IF ( DETECT_AIRCRAFT(ISENS) .EQ. 2) THEN ! can only detect gnd targets
D	    IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	      WRITE (KLINE$,"(4X,'Sensor may only detect ground targets.')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF
	    GOTO 600
	  ENDIF
	ELSE
D	  IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	    WRITE (KLINE$,"(4X,'Target is not an aircraft')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IF ( DETECT_AIRCRAFT(ISENS) .EQ. 1) THEN ! can only detect air targets
D	    IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	      WRITE (KLINE$,"(4X,'Sensor may only detect aircraft targets.')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF
	    GOTO 600
	  ENDIF
	ENDIF

		 */
	}
	
	private boolean withinFOV(double xmin, double xmax, double ymin, double ymax, double visSqr, double dview, double fov, DetectedEntity target){
		if (tracing){
			Tracer.write(Tracer.DETECTION,3,"checking if target within FOV "); 
		}

		/*
		 * 
	IF (CONICVIEW(IUNIT,ISIDE))THEN
D	  IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	      WRITE (KLINE$,"(4X,'Using conical view mode.')")
D	      CALL TRACEOUT (0,0,0)
D	  ENDIF

	  CALL SEECONIC(IUNIT,ISIDE,JUNIT,JSIDE,JTYPE, RANGESQR, ISPY)

C-------- is it within max vis range
	ELSE
D	  IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	      WRITE (KLINE$,"(4X,'Using normal view mode.')")
D	      CALL TRACEOUT (0,0,0)
D	  ENDIF

	  CALL SEENORMAL (IUNIT,ISIDE,JUNIT,JSIDE,JTYPE,RANGESQR, XT, YT, ISPY)

	ENDIF
	
	 * 
	 */
		boolean isee = seenormal(xmin, xmax, ymin, ymax, visSqr, dview, fov, target);
		if (!isee){
			if (tracing){
				Tracer.write(Tracer.DETECTION,4,"target outside FOV "); 
			}
			return false;
		}
		if (tracing){
			Tracer.write(Tracer.DETECTION,4,"target inside FOV "); 
		}
		return true;
	}
	
	private boolean withinLOS(DetectedEntity target){
		/*
C-------- do I have LOS
500	CONTINUE

	JBUILD = IN_BUILDING(JUNIT,JSIDE)
	JWALL = AT_WALL(JUNIT,JSIDE)
	JFLOOR = ON_FLOOR(JUNIT,JSIDE)
	JROOF = ON_ROOF(JUNIT,JSIDE)

	IF( RANGE .GT. 0.025 )  THEN
	  CALL SMOKELOS ( XO,YO,ZO,XT,YT,ZT,ISNSRCLAS, ISEE )
	  IF( ISEE .EQ. 0 ) THEN
D	    IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	      WRITE (KLINE$,"(8X,'Smoke blocks LOS')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF
	    GOTO 600	! Smoke blocks view ???
	  ENDIF
	ENDIF

	DEBUGLOS = 0
D	IF ( DEBUGVIEW .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	  DEBUGLOS = 1
D	ENDIF

c	TIMESTAMP1  =  SECNDS( 0.0 )

	PLOS = 0.0

c	IF ( DETECTED_TIME .LE. TMOVED(IUNIT,ISIDE) ) THEN
c	  GOTO 550
c	ENDIF

c	IF ( DETECTED_TIME .LE. TMOVED(JUNIT,JSIDE) ) THEN
c	  GOTO 550
c	ENDIF

c	PLOS = DETECTED_PLOS
c	GOTO 560

550	CONTINUE

D	IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN

D	  WRITE (KLINE$,"(4X,'Determining if observer and target are in the same building')")
D	  CALL TRACEOUT (0,0,0)

D	  IF ( IROOF .GT. 0 ) THEN
D	    WRITE (KLINE$,"(8X,'Observer is on roof of building ',I6)") IROOF
D	    CALL TRACEOUT (0,0,0)
D	  ELSE
D	    WRITE (KLINE$,"(8X,'Observer is in building ',I6)") IBUILD
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(8X,'on floor ',I6)") IFLOOR
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(8X,'and at wall ',I6)") IWALL
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF
D	  IF ( JROOF .GT. 0 ) THEN
D	    WRITE (KLINE$,"(8X,'Target is on roof of building ',I6)") JROOF
D	    CALL TRACEOUT (0,0,0)
D	  ELSE
D	    WRITE (KLINE$,"(8X,'Target is in building   ',I6)") JBUILD
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(8X,'on floor ',I6)") JFLOOR
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(8X,'and at wall ',I6)") JWALL
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF
D	ENDIF

	CALL DOLOS_BUILD ( XO,YO,ZO, XT,YT,ZT, PLOS, IBUILD, IWALL, JBUILD, JWALL, IROOF, JROOF, IFLOOR, JFLOOR )


D	IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	  WRITE (KLINE$,"(4X,'PLOS = ',F10.3)") PLOS
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF( PLOS .LE. 0.0 )  THEN
	   GOTO 600
	ENDIF
		 * 
		 */
		return true;
	}
	
	private void updateTargetList(DetectedEntity target, int level){
		
		/*
C----------------------------------------------------------------------------
C----------------------------------------------------------------------------
C UPDATE_TARGET_LIST
C Updates the detection structures based on SAR scan.
C Pre :
C----------------------------------------------------------------------------
	SUBROUTINE UPDATE_TARGET_LIST (IUNIT, ISIDE, JUNIT, JSIDE,
     *		IVIEW,
     *		ISEEHIM, ISAWHIM, NONMY,ISENS )

	include		'global.f'
	include		'globunits.f'
	include		'globdetect.f'
	include		'glbsensr.f'
	include		'globdir.f'
	include		'globdis.f'
	include		'globsar.f'
	include		'globrpt.f'

	INTEGER*4	NUM_ARRAY
	PARAMETER	(NUM_ARRAY = (NUMSIDES*NUMUNITS*NUMSIDES))

	CHARACTER*16	TIMESTRING$ /'00:00:00:00     '/
	CHARACTER*15	UTM$
	CHARACTER*30	ANSWER$

	INTEGER*4	SIDE_DETECTED(NUMSIDES, NUMUNITS, NUMSIDES)
	DATA		SIDE_DETECTED / NUM_ARRAY* 0 /

	IF ( ISENS .LE. 0 ) THEN
	  PRINT *,'WARNING: update_target_list'
	  PRINT *,'I have no idea how this has occurred.'
	  PRINT *,'The value for ISENS is ',ISENS
	  PRINT *,'IUNIT = ',IUNIT, ' ISIDE = ',ISIDE
	  PRINT *,'JUNIT = ',JUNIT, ' JSIDE = ',JSIDE
	  PRINT *,'IVIEW = ',IVIEW, ' ISEEHIM = ',ISEEHIM,' ISAWHIM = ',ISAWHIM
	  PRINT *,'I will ignore this detection event and continue,'
	  PRINT *,'but you should record this and let someone know.'
	  CALL DISPWAIT ( IVIEW, 5 )
	  CALL PRESSRET
	  CALL DISPWAIT ( IVIEW, 0 )
	  GOTO 999
	ENDIF

	ITYPE = KSYSTYP(IUNIT, ISIDE)
	ISNSR = KSENSNUM(ISENS,ITYPE)
	IFORCE = MYFORCE(IUNIT,ISIDE)

	! if obs level has changed at all, record it
	IF ( ISAWHIM .NE. ISEEHIM) THEN
	  CALL WTSNSR ( IUNIT,ISIDE, JUNIT,JSIDE, ISEEHIM, ISNSR, ISAWHIM)
	ENDIF

	! if target is seen
	IF (ISEEHIM .GT. IOBSNONE ) THEN
	  NONMY = NONMY + 1

D	  IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	    WRITE (KLINE$,"(12X,' Target spotted.',I6)") JUNIT
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(12X,'Slot ',I3,' level ',I4)") NONMY, ISEEHIM
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IF (ISNSR .LE. NUMSENSTYPS) THEN ! normal sensor

D	    IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	      WRITE (KLINE$,"(12X,'Normal optical or thermal sensor')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    KANSEE(IUNIT,ISIDE,NONMY,JSIDE,1) = JUNIT
	    KANSEE(IUNIT,ISIDE,NONMY,JSIDE,2) = ISEEHIM
	  ELSE ! mmr sensor
	    ISAR = ISARNUMBER(IUNIT, ISIDE, ISENS) ! get the sar instance number

D	    IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	      WRITE (KLINE$,"(12X,'MMR sensor # ',I4)") ISAR
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    SEEN_BY_MMR(ISAR, JUNIT, JSIDE) = 1
	  ENDIF

	  IF (KDETLVL(JUNIT,JSIDE,IFORCE) .LT. ISEEHIM ) THEN
	    KDETLVL(JUNIT,JSIDE,IFORCE) = ISEEHIM
	    KSEESME(JUNIT,JSIDE,IFORCE) = IUNIT

	    CALL WTDETEC ( IUNIT,ISIDE, JUNIT,JSIDE, ISEEHIM, ISNSR, ISAWHIM)

D	    IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	      WRITE (KLINE$,"(12X,'New detection level reported.')")
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$,"(12X,'Owner for force ',I4,' is now ',I6,x,I2)") IFORCE, IUNIT,ISIDE
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    IF ( ISEEHIM .GT. SIDE_DETECTED(ISIDE,JUNIT,JSIDE)) THEN
	      SIDE_DETECTED(ISIDE,JUNIT,JSIDE) = ISEEHIM
	      WRITE (IFILEPPJAWS) ISIDE, JUNIT, JSIDE, ISEEHIM
	    ENDIF

	    ! report new contact
	    IF (ISEEHIM .GT. IOBSRADAR ) THEN

D	      IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	        WRITE (KLINE$,"(12X,'Trigger any contact drills')")
D	        CALL TRACEOUT (0,0,0)
D	      ENDIF

	      ! trigger any contact events that might result
	      CALL TRIGGER_CONTACT (IUNIT,ISIDE)

	      IV = NTFDEV( IUNIT, ISIDE)
	      IF ( IV .GT. 0 ) THEN
	        IF ( KONTACT(IV) .EQ. KONTACTON ) THEN

		  XT = XUNIT(JUNIT,JSIDE)
	          YT = YUNIT(JUNIT,JSIDE)
c	          CONTACTX(IV) = XT
c	          CONTACTY(IV) = YT
	          CALL JANUS2UTM (XT, YT, UTM$ )
	          IF ( ISEEHIM .GE. IOBSREC .AND. NSCORE (JUNIT,JSIDE) .LE. 0 ) THEN
	          ELSE
	            CALL WHAT_DO_I_SEE (IFORCE, JUNIT, JSIDE, ISEEHIM, ANSWER$)
	            WRITE (KLINE$,"( 'Sighted ',A30,' at GR ',A15 )") ANSWER$, UTM$
	            CALL ALERT (IUNIT,ISIDE)
	          ENDIF

c		  XT = XUNIT(JUNIT,JSIDE)
c	          YT = YUNIT(JUNIT,JSIDE)
c	          CONTACTX(IV) = XT
c	          CONTACTY(IV) = YT
c	          CALL SET_VIEW_WINDOW(2,IV, IL)
c	          CALL JANUS2UTM (XT, YT, UTM$ )
c	          TIMESTRING$ = '00:00:00:00'
c	          CALL CONV_CLOCK( CLOCK, TIMESTRING$ )
c	          WRITE (KLINE$,"( A16,' Sighting at GR ',A13 )") TIMESTRING$, UTM$
c	          CALL LINEOUT (1,0,0)
c	          CALL WHAT_DO_I_SEE (IUNIT, ISIDE, JUNIT, JSIDE, ISEEHIM, ANSWER$)
c	          WRITE (KLINE$,"(A30)") ANSWER$
c	          CALL LINEOUT (0,0,0)
c	          CALL LINEOUT (2,0,0)
	        ENDIF
	      ENDIF
	    ENDIF

	    IF (IVIEW .GT. 0 ) THEN
	      IREDRAW(JUNIT,IVIEW,JSIDE) = 1
	    ENDIF
	  ENDIF
	ELSE ! iseehim = iobsnone ie not detected

D	  IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	    WRITE (KLINE$,"(12X,'Target not spotted.')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IF (ISNSR .GT. NUMSENSTYPS) THEN ! mmr sensor
	    ISAR = ISARNUMBER(IUNIT, ISIDE, ISENS) ! get the sar instance number

D	    IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	      WRITE (KLINE$,"(12X,'MMR sensor # ',I4)") ISAR
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    SEEN_BY_MMR(ISAR, JUNIT, JSIDE) = 0
	  ENDIF

D	  IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	    WRITE (KLINE$, 10009)
D     *		KSEESME(JUNIT,JSIDE,IFORCE), ISIDE,
D     *		KDETLVL(JUNIT,JSIDE,IFORCE)
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

10009	  FORMAT (12X,'Observation previously owned by icon ',
     *		I4,X,'side ',I4,' level ',I2)

	  IF (KSEESME(JUNIT,JSIDE,IFORCE) .EQ. IUNIT ) THEN

D	    IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	      WRITE (KLINE$,"(12X,'Therefore the old detection is now lost.')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    CALL WTDETEC ( IUNIT,ISIDE, JUNIT,JSIDE, ISEEHIM, ISNSR, ISAWHIM)
	    KSEESME(JUNIT,JSIDE,IFORCE) = 0
	    KDETLVL(JUNIT,JSIDE,IFORCE) = IOBSNONE

	    IBEST_DETECTION = 0
	    DO IF=1, NUMFORCES
	      JF = ((ISIDE-1)*NUMFORCES) + IF

	      IYES = 0
	      IV = ITFVIEW(5*IF,ISIDE)
	      IF ( IV .GT. 0 ) THEN
	        IF ( IAMCONWOR(IV) .EQ. 0 ) THEN
	          IYES = 1
	        ENDIF
	      ELSE
	        IYES = 1
	      ENDIF

	      IF ( IYES .EQ. 1) THEN
	        IF ( KDETLVL(JUNIT,JSIDE,JF) .GT. IBEST_DETECTION ) THEN
	          IBEST_DETECTION = KDETLVL(JUNIT,JSIDE,JF)
	        ENDIF
	      ENDIF

	    ENDDO

	    IF (IBEST_DETECTION .NE. SIDE_DETECTED(ISIDE,JUNIT,JSIDE) ) THEN
	      SIDE_DETECTED(ISIDE,JUNIT,JSIDE) = IBEST_DETECTION
	      WRITE (IFILEPPJAWS) ISIDE, JUNIT, JSIDE, IBEST_DETECTION
	    ENDIF

	    IF (IVIEW .GT. 0 ) THEN

D	      IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	        WRITE (KLINE$,"(12X,'Redrawing icon on this view ',I4)") IVIEW
D	        CALL TRACEOUT (0,0,0)
D	      ENDIF

	      IREDRAW(JUNIT,IVIEW,JSIDE) = 1 ! flag to refresh graphics if
					! workstation present
	    ENDIF
	  ELSE ! it wasnt this icon that owned the observation

D	    IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	      WRITE (KLINE$,"(12X,'Therefore the old detection is retained.')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	  ENDIF

	ENDIF

999	RETURN
	END


		 * 
		 */
		
	}
	
	private double getSize(){
		return 1.0;
		//TODO use getsize.f
		// need to get size and realsize and maybe jconclas?
		/*
	CALL GETSIZE(IUNIT,ISIDE,JUNIT,JSIDE,JTYPE, SIZE, JCONCLAS, REALSIZE, RANGESQR, IFIRED)

D	IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	  WRITE (KLINE$,"(4X,'Target apparent size = ',F12.3)") SIZE
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(4X,'but real size = ',F12.3)") REALSIZE
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	SUBROUTINE GETSIZE(IUNIT,ISIDE,JUNIT,JSIDE,JTYPE, SIZE, JCONCLAS, REALSIZE, RANGESQR, IFIRED)

	include 	'global.f'
        include 	'globunits.f'
	include 	'globdir.f'
	include 	'globmenu.f'
	include 	'globacq.f'
	include 	'glbsensr.f'
	include		'globpi.f'
	include		'globdetect.f'
	include		'globrpt.f'

CX	SIZE   =  SIZT(JTYPE,JSIDE)

	SIZE = MYSIZE(JUNIT,JSIDE)
	REALSIZE = MYREALSIZE(JUNIT,JSIDE)
	JCONCLAS  =  MYCONCLASS(JUNIT,JSIDE)

	JDEFL  =  IDEFL(JUNIT,JSIDE)
	JDEFL  =  ABS( JDEFL )

D	IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	  WRITE (KLINE$,"(8X,'Target initial size = ',F8.3)") SIZE
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	DX  =  XUNIT(IUNIT,ISIDE) - XUNIT(JUNIT,JSIDE)	! From target
	DY  =  YUNIT(IUNIT,ISIDE) - YUNIT(JUNIT,JSIDE)	! to firer
	DZ  =  ZUNIT(IUNIT,ISIDE) - ZUNIT(JUNIT,JSIDE)	! to firer

C	IF( RANGESQR .GT. 0.0 )  THEN
C	    CALL ASPECT ( DX,DY, JUNIT,JSIDE, ANGL )
C	ELSE
C	    ANGL  =  PI	! Target range is 0, set to rear shot
C	ENDIF

C---------- reduce size if target is in defilade

CX	IF ( JDEFL .GT. 0 )  THEN
C	IF ( ( JDEFL .GT. 0 ) .AND. ( ANGL .LT. ( 5* PIOVER6) ) )  THEN		! rear shots ignore defilade
CX	  JCONCLAS  =  KONCLAS(2,JTYPE,JSIDE)
CX	  IF( JDEFL .EQ. 1 )  THEN
CX	    IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
CX	      WRITE (KLINE$, "(8X,'Target in partial defilade.')")
CX	      CALL TRACEOUT (0,0,0)
CX	    ENDIF
CX	    SIZE  =  PARTIALSIZE(JTYPE,JSIDE) ! SIZE * 0.33
CX	  ELSE
CX	    IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
CX	      WRITE (KLINE$, "(8X,'Target in full defilade.')")
CX	      CALL TRACEOUT (0,0,0)
CX	    ENDIF
CX	      SIZE  =  DEFILADESIZE(JTYPE, JSIDE) ! SIZE * 0.2
CX	  ENDIF

CX	  IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
CX	    WRITE (KLINE$,"(8X,'New size = ',F8.3)") SIZE
CX	    CALL TRACEOUT (0,0,0)
CX	  ENDIF

CX	ELSE
CX	  JCONCLAS  =  KONCLAS(1,JTYPE,JSIDE)
CX	  IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
CX	    WRITE (KLINE$, "(8X,'Target not in defilade.')")
CX	    CALL TRACEOUT (0,0,0)
CX	  ENDIF
CX	ENDIF

D	IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	  WRITE (KLINE$, "(8X,'Thermal con class ',I3)") JCONCLAS
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

CX	REALSIZE = SIZE

	IF( FLAST(JUNIT,JSIDE) .GT. (CLOCK-0.1667) ) THEN
	  SIZE  =  SIZE * 5

D	  IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	    WRITE (KLINE$,"(8X,'Target has recently fired, new size = ',F8.3)") SIZE
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	ELSE

D	  IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	    WRITE (KLINE$, "(8X,'Target has not fired recently.')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	ENDIF

C---------- increase size if target has moved recently
CX	IF( SPDU(JUNIT,JSIDE) .GT. 0.0 )  THEN
CX	  JCONCLAS  =  KONCLAS(1,JTYPE,JSIDE)

CX	  IF (ICRAWL(JUNIT,JSIDE) .NE. 0 ) THEN
CX	    SIZE  =  CRAWLSIZE(JTYPE,JSIDE) ! SIZE * 0.33
CX	    IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
CX	      WRITE (KLINE$, "(8X,'Target is crawling.')")
CX	      CALL TRACEOUT (0,0,0)
CX	      WRITE (KLINE$,"(8X,'new size = ',F8.3)") SIZE
CX	      CALL TRACEOUT (0,0,0)
CX	    ENDIF
CX	  ELSE
CX	    SIZE  =  SIZE * 1.75
CX	    IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
CX	      WRITE (KLINE$,"(8X,'Target has recently moved')")
CX	      CALL TRACEOUT (0,0,0)
CX	      WRITE (KLINE$,"(8X,'new size = ',F8.3)") SIZE
CX	      CALL TRACEOUT (0,0,0)
CX	    ENDIF
CX	  ENDIF

CX	ELSE
CX	  IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
CX	    WRITE (KLINE$, "(8X,'Target not moving.')")
CX	    CALL TRACEOUT (0,0,0)
CX	  ENDIF

CX	ENDIF

	RETURN
	END


		 * 
		 */
	}
	
	private double getRangeSqr(DetectedEntity target){
		return 1.0;
	}
	
	private boolean seenormal(double xmin, double xmax, 
			double ymin, double ymax, 
			double visSqr, double dview, double fov,
			DetectedEntity target){
		if (!withinVisBox(xmin, xmax, 
				ymin, ymax, 
				target)){
			if (tracing){
				Tracer.write(Tracer.DETECTION,4,"target outside visbox");
			}
			return false;
		}
		double dx = myEntity.getLocation().getX() - target.getLocation().getX();
		double dy = myEntity.getLocation().getY() - target.getLocation().getY();
		double dz = myEntity.getTopHeightASL() - target.getTopHeightASL();
		double rangeSqr = dx* dx + dy * dy + dz * dz;
		if (tracing){
			Tracer.write(Tracer.DETECTION,4,"x distance = " + dx);
			Tracer.write(Tracer.DETECTION,4,"y distance = " + dy);
			Tracer.write(Tracer.DETECTION,4,"z distance = " + dz);
			Tracer.write(Tracer.DETECTION,4,"range = " + Math.sqrt(rangeSqr));
		}
		if (rangeSqr > visSqr){
			if (tracing){
				Tracer.write(Tracer.DETECTION,4,"target beyond max range");
			}
			return false;
		}
		if (rangeSqr <= 0.000001){
			if (tracing){
				Tracer.write(Tracer.DETECTION,4,"observer is on top of target");
			}
			return true;
		}
		//TODO all this angle stuff should be a utility function
		double angle = Math.atan2(dy, dx);
		double diff = angle - dview;
		while (diff > Math.PI) diff = diff - (Math.PI*2);
		while (diff < -(Math.PI)) diff = diff + (Math.PI*2);
		if (tracing){
			Tracer.write(Tracer.DETECTION,4,"angle from observer to target " + angle);
			Tracer.write(Tracer.DETECTION,4,"observer direction of view " + dview);
			Tracer.write(Tracer.DETECTION,4,"observer half angle of view " + fov);
			Tracer.write(Tracer.DETECTION,4,"difference between direction of view and direction to target " + diff);
		}
		
		if (diff <= fov){
			if (tracing){
				Tracer.write(Tracer.DETECTION,4,"target inside angle of view ");
			}
			return true;
		}
		if (tracing){
			Tracer.write(Tracer.DETECTION,4,"target outside angle of view ");
		}

		return false;
		/*
	SUBROUTINE SEENORMAL(IUNIT,ISIDE,JUNIT,JSIDE,JTYPE, RANGESQR, XT, YT, ISPY)

	  ELSEIF( RANGESQR .GT. 0.000001 )  THEN
	    CALL ASPECT2 ( DX,DY, IUNIT,ISIDE, ANGL )

D	    IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	      WRITE (KLINE$, "(8X,'Angle to target ',F12.3,' radians or ',F12.3,' degrees')") ANGL, ANGL*RAD2DEG
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    IF( ANGL .GT. AL )  THEN

D	      IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	        WRITE (KLINE$, "(8X,'Outside view ')")
D	        CALL TRACEOUT (0,0,0)
D	      ENDIF

	    ELSE

D	      IF (DEBUGDTEC .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	        WRITE (KLINE$, "(8X,'Inside view ')")
D	        CALL TRACEOUT (0,0,0)
D	      ENDIF

	      ISPY = .TRUE.
	    ENDIF
	  ELSE
	  ENDIF
	ENDIF
		 * 
		 */
	}
	
	private boolean withinVisBox( double xmin, double xmax, 
			double ymin, double ymax, 
			DetectedEntity target){
		if (tracing){
			Tracer.write(Tracer.DETECTION,4,"target location " + 
					target.getLocation().toString());
		}
		if (target.getLocation().getX()<xmin) return false;
		if (target.getLocation().getX()>xmax) return false;
		if (target.getLocation().getY()<ymin) return false;
		if (target.getLocation().getY()>ymax) return false;
		return true;
	}
	
}
