package models.directfire;

import java.util.Vector;
import data.csd.PHTable;
import data.csd.Platform;
import data.csd.Weapon;
import data.map.Coordinate;
import models.Event;
import sim.Constants;
import sim.entity.FirerEntity;
import sim.entity.TargetEntity;
import utils.Parser;
import utils.Tracer;
import utils.Utils;

public class DirectFireImpactEvent extends Event {
	
	public DirectFireImpactEvent(double time,
			FirerEntity e1, TargetEntity e2,
			Weapon weapon,
			int rounds,
			double range){
		super(time);
		myFirer = e1;
		myTarget = e2;
		myRounds = rounds;
		myWeapon = weapon;
		myRange = range;
	}

	private FirerEntity myFirer;
	private TargetEntity myTarget;
	private int myRounds;
	private Weapon myWeapon;
	private double myRange;
	private Platform myFirerPlatform;
	private Platform myTargetPlatform;
	private boolean tracing;

	public Event doEvent(){
		// based on janus.dfimpact_area.f
		if (invalid()) return null;
		tracing = myFirer.isTracing();
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,0,"direct fire event time: " + 
					this.getTime());
			Tracer.write(Tracer.DIRECTFIRE,0,"direct fire event time: " + 
					Parser.formatTime(this.getTime()));
			}
		myFirerPlatform = myFirer.getPlatform();
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,0,"firer " + myFirer.getName() + " : " + myFirerPlatform.getName());
			Tracer.write(Tracer.DIRECTFIRE,0,"number of rounds: " + myRounds);
		}
		if ( myWeapon == null){
			if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"invalid firer weapon");}
			return null;
		}
		String weaponName = myWeapon.getName();
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"weapon: " + weaponName);}
		
		if ( myTarget == null){
			doAreaShot(); //CALL DFMPACT_AREA_UNAIMED ( ISLOT, IUNIT, ISIDE )
		} else {
			doAimedShot(); //CALL DFMPACT_AREA_AIMED ( ISLOT, IUNIT, ISIDE )
		}
		return null; // no reason to ever schedule a new event
	}
	
	private void doAreaShot(){}
	
	private void doAimedShot(){ //dfimpact_area_aimed.f
		myTargetPlatform = myTarget.getPlatform();
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,0,"target " + myTarget.getName() + " : " + myTargetPlatform.getName());
			Tracer.write(Tracer.DIRECTFIRE,0,"range " + myRange);
		}
		updateGUI();
		
		if (testGuided()) { 
			if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"target lost");}
		} else {
			resolveDF(); //CALL RESOLVE_DF_AREA
			applyPartialKills();
		}
	}
	
	public void resolveDF(){ //TDOD add arguments
		int vulnerability = doVul();

		int phPosture = Utils.getPHPosture(myRange, myFirer, myTarget);
		double phit = getPH(myWeapon, myTargetPlatform, myRange, phPosture);
		double pkil = getPK(myWeapon, myTargetPlatform, myRange);
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,0,"basic ph = " + phit);
			Tracer.write(Tracer.DIRECTFIRE,0,"basic pk = " + pkil);
		}
		
		phit = adjustPH(phit, myFirer, myTargetPlatform, myWeapon );
		Tracer.write(Tracer.DIRECTFIRE,0,"final ph = " + phit);
		doPit(); // probably should be within adjustph
		
		Coordinate aimpoint = selectAimPoint(myTarget);
		double xaim = aimpoint.getX();
		double yaim = aimpoint.getY();
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"aim point " + xaim + " : " + yaim);}
		calcMinMax();

		int numImpacts = myRounds;
		Vector<Double> ximpact = new Vector<Double>();
		Vector<Double> yimpact = new Vector<Double>();
		plotImpacts(numImpacts, 100.0, 100.0, phit, pkil, vulnerability, ximpact, yimpact);

		calculateEffects(ximpact, yimpact);
		write();
	}

	//TODO
	private boolean testGuided(){
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"**GUIDED WEAPONS NOT YET IMPLEMENTED**");}
		return false;
		//CALL TEST_GUIDED (IUNIT, ISIDE, IWPN, JUNIT, JSIDE, IOK)

	}
	private double getPH(Weapon weapon, Platform platform, double range, int phPosture){
		//getphpk.f
		PHTable  phTable = weapon.getPlatformPH(platform.getName());
		if (phTable==null) {
			if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,
					"no ph table assigned to this weapon vs target combination");}
			return 0.0;
		}
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,0,"using ph table " + phTable.getName());
			Tracer.write(Tracer.DIRECTFIRE,0,"range " + range);
			Tracer.write(Tracer.DIRECTFIRE,0,"maximum range for weapon " + weapon.getMaxRange());
		}
		if (range > weapon.getMaxRange()){
			if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"target beyond max range");}
			return 0.0;
		}
		testAltitude(); //TODO
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"ph posture " + phPosture);}
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,phTable.toLog());}
		double phit = phTable.getProb(phPosture, range);
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"ph " + phit);}
		
		return phit;
	}
	private double getPK(Weapon weapon, Platform platform, double range){
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"**PK LOOKUP NOT YET IMPLEMENTED**");}
		return 0.5;
	}
	
	private void testAltitude(){
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"**ALTITUDE NOT YET IMPLEMENTED**");}

		/*
	JFLY = FLYERS(JTYPE)

	CALL UNITXY ( JUNIT,JSIDE, XT,YT )
	CALL UNITXY ( IUNIT,ISIDE, XF,YF )

	DX  =  XF - XT	! From target
	DY  =  YF - YT	! to firer

	CALL UNIT_SENSOR_HEIGHT_ASL(IUNIT, ISIDE, ZF)
	CALL UNIT_TARGET_HEIGHT_ASL(JUNIT, JSIDE, ZT)

	DZ  =  ZF - ZT	! to firer

D	IF (DEBUGDFIR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	  WRITE (KLINE$,"(16X, 'Target is ',F12.3,'m higher than firer')") -DZ
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(16X, 'Maximum altitude for weapon = ',F12.3,'m')") WEAPON_MAX_ALTITUDE(IWEAPN)
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF ( -DZ .GT. WEAPON_MAX_ALTITUDE(IWEAPN) ) THEN

D	  IF (DEBUGDFIR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	    WRITE (KLINE$,"(16X, 'Maximum altitude for wpn exceeded')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  GOTO 999
	ENDIF

	IF ( WEAPON_MIN_ALTITUDE(IWEAPN) .GT. 0.0 ) THEN
	  CALL UNIT_BASE_HEIGHT_AGL ( JUNIT, JSIDE, Z )
	  ALT = TRRNVEGH(XT,YT)

D	  IF (DEBUGDFIR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	    WRITE (KLINE$,"(16X, 'Target height above ground ',F12.1,'m')") Z
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(16X, 'Vegetation height ',F12.1,'m')") ALT
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(16X, 'Minimum altitude for weapon = ',F12.3,'m')") WEAPON_MIN_ALTITUDE(IWEAPN)
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  Z = Z - ALT
	  IF ( Z .LT. WEAPON_MIN_ALTITUDE(IWEAPN)) THEN

D	    IF (DEBUGDFIR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	      WRITE (KLINE$,"(16X, 'Minimum altitude for wpn failed')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    GOTO 999
	  ENDIF
	ENDIF
		 * 
		 */
	}
	
	private double adjustPH(double phit, FirerEntity firer, 
			Platform targetPlatform, Weapon weapon){
		if (firer.isSuppressed()){
			phit = phit * targetPlatform.getSuppressionFactorPH();
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,0,"firer is suppressed");
				Tracer.write(Tracer.DIRECTFIRE,0,"modified ph = " + phit);
			}
		} else {
			if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"firer is not suppressed");}
		}
		
		if (firer.getMOPP()){ //TODO
			phit = phit * weapon.getMoppFactor();
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,0,"firer is in MOPP");
				Tracer.write(Tracer.DIRECTFIRE,0,"mopp factor = " + weapon.getMoppFactor());
				Tracer.write(Tracer.DIRECTFIRE,0,"modified ph = " + phit);
			}
		}
		doPLOS();
		return phit;
	}
	
	private void doPit(){
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"**PIT EFFECT NOT YET IMPLEMENTED**");}
		/*
		 * 
	IF( IN_PIT(JUNIT,JSIDE) .GT. 0 )   THEN

	  JVUL = ARTILLERY_VULNERABILITY(4,JTYPE)

	  PHIT = PHIT * PIT_PH(IWEAPN,JVUL)
	  PKIL = PKIL * PIT_PK(IWEAPN,JVUL)

D	  IF (DEBUGDFIR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	    WRITE (KLINE$,"(16X, 'Target is in a pit')")
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(16X, 'Apply pit modifiers ',F6.3,XX,F6.3)")
D     *			PIT_PH(IWEAPN, JVUL), PIT_PK(IWEAPN, JVUL)
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(16X, 'Vulnerability cat ',I3)") JVUL
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(16X, 'Modified PH = ',F10.3)") PHIT
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(16X, 'Modified PK = ',F10.3)") PKIL
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	ENDIF
		 */
	}
	
	private void doPLOS(){
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"**PLOS EFFECT NOT YET IMPLEMENTED**");}
		
		/*getphpk
		 * 
C	Modify PH due to the type of surface cover surrounding target.

	IF ( JFLY ) THEN ! target is a flyer - no plos mods

D	  IF (DEBUGDFIR .AND. ITRACK(IUNIT,ISIDE) ) THEN
D	    WRITE (KLINE$,"(16X, 'Target is an aircraft.')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	ELSE

c	  CALL TRRNPLOS ( XT, YT, HEIGHT, PLOS )

c	  IF (DEBUGDFIR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
c	    WRITE (KLINE$,"(16X, 'Target plos = ',F10.3)") PLOS
c	    CALL TRACEOUT (0,0,0)
c	  ENDIF
C	  PHIT = PHIT * PLOS
C	  IF (DEBUGDFIR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
C	    WRITE (KLINE$,"(16X, 'Modified PH = ',F10.3)") PHIT
C	    CALL TRACEOUT (0,0,0)
C	  ENDIF
	ENDIF

	IGROUP = MYFORCE(IUNIT,ISIDE)
	IF (KDETLVL(JUNIT,JSIDE,IGROUP) .LT. IOBSREC ) THEN
	  PHIT = PHIT * UNRECPH

D	  IF (DEBUGDFIR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	    WRITE (KLINE$,"(16X, 'Target not recognised - Modified PH = ',F10.3)") PHIT
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	ELSE

D	  IF (DEBUGDFIR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	    WRITE (KLINE$,"(16X, 'Target recognised - no effect on PH')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	ENDIF

	IBUILD = IN_BUILDING(IUNIT,ISIDE)
	IWALL = AT_WALL(IUNIT,ISIDE)
	IROOF = ON_ROOF(IUNIT,ISIDE)
	IFLOOR = ON_FLOOR(IUNIT,ISIDE)

	JBUILD = IN_BUILDING(JUNIT,JSIDE)
	JWALL = AT_WALL(JUNIT,JSIDE)
	JROOF = ON_ROOF(JUNIT,JSIDE)
	JFLOOR = ON_FLOOR(JUNIT,JSIDE)

	CALL DOLOS_BUILD ( XF,YF,ZF, XT,YT,ZT, PLOS, IBUILD, IWALL, JBUILD, JWALL, IROOF, JROOF, IFLOOR, JFLOOR )
	PLOS_MAX = LOS_MAX_THRESHHOLD(IWEAPN,ILOS_MOVE)
	PLOS_MIN = LOS_MIN_THRESHHOLD(IWEAPN,ILOS_MOVE)
	PLOS_RANGE = PLOS_MAX - PLOS_MIN

c	    LOS_MIN_THRESHHOLD(ITO,ISIDE) = LOS_MIN_THRESHHOLDC(IFROM)
c	    LOS_FACTOR(ITO,ISIDE) = LOS_FACTORC(IFROM)
c	    WIRE_MAX_THRESHHOLD(ITO,ISIDE) = WIRE_MAX_THRESHHOLDC(IFROM)
c	    WIRE_MIN_THRESHHOLD(ITO,ISIDE) = WIRE_MIN_THRESHHOLDC(IFROM)
c	    WIRE_FACTOR(ITO,ISIDE) = WIRE_FACTORC(IFROM)

D	IF (DEBUGDFIR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	  WRITE (KLINE$,"(16X, 'PLOS from firer to target = ',F6.3)") PLOS
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(16X, 'MAX threshhold for wpn    = ',F6.3)") PLOS_MAX
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(16X, 'MIN threshhold for wpn    = ',F6.3)") PLOS_MIN
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF ( PLOS .LT. PLOS_MAX ) THEN
	  IF ( PLOS .LT. PLOS_MIN ) THEN
	    PHIT = 0.0

D	    IF (DEBUGDFIR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	      WRITE (KLINE$,"(16X, 'PH set to 0.0 due to PLOS')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	  ELSE
	    PHIT = PHIT * ((PLOS-PLOS_MIN)/PLOS_RANGE) * LOS_FACTOR(IWEAPN,ILOS_MOVE)

D	    IF (DEBUGDFIR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	      WRITE (KLINE$,"(16X, 'Calculating linear iterpolation of plos factor')")
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$,"(16X, 'PLOS_FACTOR  = ',F6.3)") LOS_FACTOR(IWEAPN,ILOS_MOVE)
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$,"(16X, 'PR= PLOS_MAX - PLOS_MIN = ',F6.3)") PLOS_MAX-PLOS_MIN
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$,"(16X, 'PF = PLOS - min threshold = ',F6.3)") PLOS-PLOS_MIN
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$,"(16X, 'PF/PR = ',F6.3)") (PLOS-PLOS_MIN)/PLOS_RANGE
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$,"(16X, 'PF/PR * FACTOR = ',F6.3)") ((PLOS-PLOS_MIN)/PLOS_RANGE)*LOS_FACTOR(IWEAPN,ILOS_MOVE)
D	      CALL TRACEOUT (0,0,0)
D	      WRITE (KLINE$,"(16X, 'PH set to ',F6.3', due to PLOS')") PHIT
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	  ENDIF
	ELSE

D	  IF (DEBUGDFIR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	    WRITE (KLINE$,"(16X, 'PH not altered by PLOS')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	ENDIF
		 */
	}
	
	private Coordinate selectAimPoint(TargetEntity target){
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"calculating aim point");}
		Vector<Coordinate> elementCoordinates = target.getElementLocations();
		int elements = elementCoordinates.size();
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"target has " + elements + " elements");}
		if (elements == 1){
			return elementCoordinates.get(0);
		} else {
			int i = (int)(elements*Math.random());
			if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"using element " + (i+1));}
			return elementCoordinates.get(i);
		}
	}
	
	private int doVul(){
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"**VULNERABILITY CATEGORIES NOT YET IMPLEMENTED**");}
		return 1;
		/*
	JDEF = IDEFL(JUNIT,JSIDE)

	IF ( IN_PIT(JUNIT,JSIDE) .GT. 0 ) THEN
	  JVUL  =  ARTILLERY_VULNERABILITY(IDEFFUL+2,JTYPE)
	ELSEIF ( JDEF .GE. 0 .AND. JDEF .LE. IDEFFUL) THEN
	  JVUL  =  ARTILLERY_VULNERABILITY(JDEF+1,JTYPE)
	ELSE
	  JVUL = 0
	ENDIF

D	IF (DEBUGDFIR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	  WRITE (KLINE$,"('Target vulnerability class ',I4)") JVUL
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF (JVUL .LE. 0 .OR. JVUL .GT. NVULCAT ) THEN
	  TYPE *
	  TYPE *,'ERROR - RESOLVE_DF can not fire a shot.'
	  TYPE *,'System type: ',SYSTNAME$(JTYPE),
     *			' has no vulnerability class.'
	  TYPE *
	  NFIR = 0
	  GOTO 999
	ENDIF

		 * 
		 */
	}
	
	private void write(){
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"writing pp data");}
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"**PP DATA NOT YET IMPLEMENTED**");}
		/*
	CALL WRITE_DF ( IUNIT, ISIDE, JUNIT, JSIDE,
     *		PHIT, PKIL, NUMROUNDS, IHITS, IKILLS,
     *		RANGE, IPHSTAT, IWPN )

		 * 
		 */
	}
	
	private void calculateEffects(Vector<Double> ximpact, Vector<Double> yimpact){
		int numImpacts = ximpact.size();
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"calculating effect of " + numImpacts + " impacts");}
		/*
	IF ( NUMIMPACTS .GT. 0 ) THEN
	  CALL RESOLVE_IMPACT (IUNIT, IWPN, ISIDE, RANGE, ITIMPACT,
     *		NUMIMPACTS, XIMPACT, YIMPACT, IHITS, IKILLS, JUNIT, JSIDE, IELEM, IEIMPACT,
     *		XIMPACT_MIN, YIMPACT_MIN, XIMPACT_MAX, YIMPACT_MAX, IBIMPACT, IFLOOR)
	ENDIF

D	IF ( DEBUGDFIR .EQ. 1 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	  WRITE (KLINE$,"('Total number of hits  = ',I4)") IHITS
D	  CALL TRACEOUT (0,1,0)
D	  WRITE (KLINE$,"('Total number of kills = ',I4)") IKILLS
D	  CALL TRACEOUT (0,0,0)
D	ENDIF
		 * 
		 */
	}
	private boolean testPointDefence(){
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"**POINT DEFENCE NOT YET IMPLEMENTED**");}
		return false;
	/*
	  IRESULT = 0
	  CALL TEST_POINT_DEFENCE( IUNIT, ISIDE, IWPN, JUNIT, JSIDE, IRESULT )
	  IF ( IRESULT .GT. 0 ) THEN
	    NUMIMPACTS = NUMIMPACTS -1

D	    IF (DEBUGDFIR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	      WRITE (KLINE$,"('Round stopped by point defence system ')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    GOTO 800
	  ENDIF

	 * 
	 */
	}
	
	private int getResult(double phit, double pkil){
		return 0; //TODO
	}
	
	private void plotImpacts(int numImpacts, double xaim, double yaim, 
			double phit, double pkil, int vulnerability, 
			Vector<Double> ximpact,
			Vector<Double> yimpact){
		int impacts = 0;
		for (int i=0;i<numImpacts;i++){
			if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"locating impact point for round " + (i+1));}
			//	  ITRRN = ITRRNSURF(XAIM, YAIM) + 1
			if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"aim point ");}
			if (testPointDefence()){
				if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"round stopped by point defence ");}
			} else {
				if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"point defence ineffective");}
				impacts++;
				int result = getResult(phit, pkil);
				/*
	  CALL GET_IMPACT_POINT (IWPN, ISIDE, JVUL, ITRRN,
     *		IRESULT, XAIM, YAIM, XLOC, YLOC, IDEBUG)

	  ITIMPACT(IRND) = ITRRN
	  IEIMPACT(IRND) = IRESULT
				 * 
				 */
				ximpact.addElement(xaim);
				yimpact.addElement(yaim);
				
				/*
	  CALL TRRN_IN_BUILDING(IBUILD, XLOC, YLOC, IDEBUG)
	  IBIMPACT(I) = IBUILD

D	  IF ( DEBUGDFIR .EQ. 1 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	    WRITE (KLINE$,"(4X,'Round impact point ',F14.5,XX,F14.5)") XLOC, YLOC
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(4X,'In building ',I4)") IBUILD
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IF ( XLOC .LT. XIMPACT_MIN ) THEN
	    XIMPACT_MIN = XLOC
	  ENDIF
	  IF ( YLOC .LT. YIMPACT_MIN ) THEN
	    YIMPACT_MIN = YLOC
	  ENDIF

	  IF ( XLOC .GT. XIMPACT_MAX ) THEN
	    XIMPACT_MAX = XLOC
	  ENDIF
	  IF ( YLOC .GT. YIMPACT_MAX ) THEN
	    YIMPACT_MAX = YLOC
	  ENDIF
				 * 
				 */
			}
		}
	}
	
	private void calcMinMax(){
		/*
		 * 	/*

	! calculate the min and max for the box containing the impacts
	! set min to max terrain size and max to min terrain size
	! then compare with each impact as it is plotted

	CALL TRRN_COORDS (XIMPACT_MAX, YIMPACT_MAX, XIMPACT_MIN, YIMPACT_MIN)

	IFLOOR = 0
	JBUILD = 0
	IF ( IN_BUILDING(JUNIT,JSIDE) .GT. 0 ) THEN
	  IFLOOR = ON_FLOOR(JUNIT,JSIDE)
	  JBUILD = IN_BUILDING(JUNIT,JSIDE)
	ELSEIF ( ON_ROOF(JUNIT,JSIDE) .GT. 0 ) THEN
	  JBUILD = ON_ROOF(JUNIT,JSIDE)
	  CALL TRRN_GET_BUILD_FLOORS(JBUILD, IFLOOR)
	ENDIF

D	IF (DEBUGDFIR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	  WRITE (KLINE$,"('Target is on floor ',I3,' of bld ',I4)") IFLOOR, JBUILD
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IRND = 0
	
	*/
	}
	
	private boolean invalid(){
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"**NEED TO VALIDATE EVENT DATA**");}
		return false;
		//TODO validate
		/*
		 * 	IUNIT   =  IFIRQ(1,ISLOT,ISIDE)		! Unit number of firer
		 * 	IF ( IUNIT .LT. 1 )  GOTO 999
		 */
	}
	
	private void updateGUI(){
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"**UPDATING GUI NOT YET IMPLEMENTED**");}
		/*
		 * C------- Enter direct fire event into graphics queue.
		 * IFIRER  =  IUNIT
		 * IF( MOUNTED(IUNIT,ISIDE) .GT. 0 )  IFIRER = MOUNTED(IUNIT,ISIDE)
		 * CALL TRAJDRW ( IFIRER, ISIDE, JUNIT ,JTSID )
		 * 
		 */

	}

	private void applyPartialKills(){
		if (tracing){Tracer.write(Tracer.DIRECTFIRE,0,"**APPLY PARTIAL KILLS NOT YET IMPLEMENTED**");}
		/*
		 * IKILLS = KILLS
		 * CALL APPLY_PARTIAL_KILLS (JUNIT, JTSID, IUNIT, ISIDE, IWPN, NONMOVE, NONFIRE)
		 */
	}
}

