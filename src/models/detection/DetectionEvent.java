package models.detection;

import data.csd.Sensor;
import models.Event;
import sim.GameClock;
import sim.Scenario;
import sim.entity.Entity;
import sim.entity.ObserverEntity;
import utils.Logger;
import utils.Parser;
import utils.Tracer;

public class DetectionEvent extends Event{

	private Scenario myScenario;
	private ObserverEntity myEntity;
	private GameClock gameClock;

	public DetectionEvent(double eventTime, Scenario scenario, GameClock clock, Entity entity) {
		super(eventTime);
		myScenario = scenario;
		myEntity = entity;
		gameClock = clock;
	}

	/**
	 * The method to execute processing of a detection event.
	 */
	@Override
	public Event doEvent(){
		tracing = this.myEntity.isTracing();		
		if (tracing){
			Tracer.write(Tracer.DETECTION,0,"\nprocessing detection ");
			Tracer.write(Tracer.DETECTION,0,"for entity " + myEntity.getID());
			Tracer.write(Tracer.DETECTION,0,"event time: " + 
					Parser.formatTime(this.getTime()) + "  " + this.getTime());
			Tracer.write(Tracer.DETECTION,0,"game clock: " + 
					Parser.formatTime(gameClock.getClock()) + 
					"  " + gameClock.getClock());
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
			Tracer.write(Tracer.DETECTION,1,"will hand of old targets to new sensor");
		}
	}
	
	private void detect(){
		/*
		 * 
	SUBROUTINE  DETECTIONS ( IUNIT, ISIDE, NONMY, CTICK, JSIDE )


	include		'global.f'
	include		'globunits.f'
	include		'globsrch.f'
	include		'globsar.f'
	include		'glbnames.f'
	include		'globrpt.f'

	PARAMETER (NUM_ARRAY = NUMUNITS*NUMSIDES)

	INTEGER*4	LAST_SENSOR(NUMUNITS,NUMSIDES) / NUM_ARRAY*0 /
	CHARACTER*3	SAR_MSG$, GMTI_MSG$

	ITYPE = KSYSTYP(IUNIT,ISIDE)
        IVIEW = NTFDEV(IUNIT,ISIDE)

	*/
		Sensor sensor = this.myEntity.getCurrentSensor();
		if (tracing){
			if (sensor == null){
				Tracer.write(Tracer.DETECTION,1,"no active sensor ");
			} else {
				Tracer.write(Tracer.DETECTION,1,"using sensor " + sensor.getName());
				Tracer.write(Tracer.DETECTION,1,"sensor band " + 
						Sensor.SENSOR_BANDS[sensor.getBand()]);
			}
		}
		changeSensor (sensor);
		
	/*

	! now perform detections based on the sensor band

	IF (IBAND .LE. 0 ) THEN

D	  IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	    WRITE (KLINE$,"('No valid sensor found. No further processing.')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	ELSEIF (IBAND .LT. IBAND_SAR1 ) THEN

D	  IF (DEBUGDTEC .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GE. 1 ) THEN
D	    WRITE (KLINE$,"('Optical and thermal model')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  CALL FINDNMY ( IUNIT, ISIDE, NONMY, CTICK, JSIDE, ISENS, ISTYP )

c	  CTICK = DTSEARCH
C	  IF ( NONMY .LE. 0 ) THEN
C	    CALL CLEAR_DETECTED_TARGETS(IUNIT,ISIDE,IVIEW,JSIDE,NONMY,ISENS)
C	  ENDIF
c	  IF ( JSIDE .NE. ISIDE .AND. DODETECT_OBST .EQ. 1) THEN
c	    CALL DETECT_MINES (IUNIT, ISIDE)
c	    CALL DETECT_OBSTACLES (IUNIT, ISIDE)
c	  ENDIF

	ELSE IF ( IBAND .EQ. IBAND_SAR1 ) THEN
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

 	ENDIF
	LAST_SENSOR(IUNIT,ISIDE) = ISENS
		 */
		this.myEntity.setLastSensor(sensor);
	}
	
}
