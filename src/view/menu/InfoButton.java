package view.menu;

import data.map.Coordinate;
import sim.Scenario;
import sim.entity.Entity;
import utils.Logger;

public class InfoButton extends MenuButton {

	public InfoButton() {
		super("Info");
	}

	public void clickMap(Coordinate coordinate){
		//Logger.say("InfoButton: hit map " + coordinate.toString());
		Entity entity = ((MenuController)menuGroup).getEntity(coordinate);
		if (entity == null) return; // TODO other reports
		((MenuController)menuGroup).report(getInfoReport(entity));
	}
	
	private String getInfoReport(Entity entity){
		// See prtunit.f
		
		String s = "";
		s = s + entity.getLocation().toGrid() + "\n";
		s = s + entity.getName() + "\n";
		if (entity.getForce()!=null){
			s = s + "Force: " + entity.getForce().getName();
		} else {
			s = s + "no force defined";
		}
		s = s + "   Group: " + entity.getGroup() + "\n";
		
		s = s + "Strength: initial " + entity.getOriginalElements() +
				"  current " + entity.getNumberOfElements() + "\n";
		/*
		 * 
	PARAMETER	FACTR1  =  0.53996	! Converts Km/Hr to Knots
	PARAMETER	FACTR2  =  1.85200	! Converts Knots to Km/Hr

	IF( KINOPSTAT(IUNIT,ISIDE) .NE. 0  )  THEN
	  WRITE (KLINE$,"('Inopperable due to chemical effects')")
	  CALL MSGOUT (IVIEW,0,0,0)
	ELSE
c	  WRITE (KLINE$,"('Not affected chemical effects')")
c	  CALL MSGOUT (IVIEW,0,0,0)
	ENDIF

	IF ( NONMOVERS(IUNIT,ISIDE) .GT. 0 ) THEN
	  WRITE (KLINE$,"('Mobility kills   = ',I4)") NONMOVERS(IUNIT,ISIDE)
	  CALL MSGOUT (IVIEW,0,0,0)
	ENDIF

	IF ( NONFIRERS(IUNIT,ISIDE) .GT. 0 ) THEN
	  WRITE (KLINE$,"('Fire Power kills = ',I4)") NONFIRERS(IUNIT,ISIDE)
	  CALL MSGOUT (IVIEW,0,0,0)
	ENDIF

	IF( TSUPRS(IUNIT,ISIDE) .GT. 0 )THEN
	  WRITE (KLINE$,"('Unit suppression factor ',F10.3,'%')") TSUPRS(IUNIT,ISIDE)*100
	  CALL MSGOUT (IVIEW,0,0,0)
	ENDIF


C	IF (ROUTE_LOCK(IUNIT,ISIDE) .GT. 0 ) THEN
C	  WRITE (KLINE$,"('Icon is subject to route locks')")
C	  CALL MSGOUT (IVIEW,0,0,0)
C	ELSE
C	  WRITE (KLINE$,"('Icon is NOT subject to route locks')")
C	  CALL MSGOUT (IVIEW,0,0,0)
C	ENDIF



	IF (IFLYTYPE .GT. 0) THEN
	  IF (IFLYMODE(IUNIT,ISIDE) .EQ. IFLYAGL ) THEN
	    WRITE (KLINE$,"('     Set alt ',I5,'ft ',I5,'m AGL')")
     *		NINT(ALTITUDE(IUNIT,ISIDE)*METRE2FEET), NINT(ALTITUDE(IUNIT,ISIDE))
	    CALL MSGOUT (IVIEW,0,0,0)
	  ELSE
	    WRITE (KLINE$,"('     Set alt ',I5,'ft ',I5,'m ASL')")
     *		NINT(ALTITUDE(IUNIT,ISIDE)*METRE2FEET), NINT(ALTITUDE(IUNIT,ISIDE))
	    CALL MSGOUT (IVIEW,0,0,0)
	  ENDIF

	  CALL TRRNELEV2( XUNIT(IUNIT,ISIDE),YUNIT(IUNIT,ISIDE), ALT )
	  ALT = ALT + TRRNVEGH(XUNIT(IUNIT, ISIDE),YUNIT(IUNIT,ISIDE))
	  WRITE (KLINE$,"('Feature Alt  ',I5,'ft  ',I5,'m ASL')") NINT(ALT * METRE2FEET), NINT(ALT)
	  CALL MSGOUT (IVIEW,0,0,0)

	  WRITE (KLINE$,"(' Actual alt ',I5,'feet   ',I5,'m ASL')") 
     *		NINT((ZUNIT(IUNIT,ISIDE)*1000.0)*METRE2FEET), NINT(ZUNIT(IUNIT,ISIDE)*1000.0)
	  CALL MSGOUT (IVIEW,0,0,0)
	ENDIF

	IF (IFLYTYPE .GT. 0 ) THEN
	  WRITE (KLINE$,"('Speed set to ',F8.2,'knots')") SETSPEED(IUNIT,ISIDE) * KM2KNOTS
	  CALL MSGOUT (IVIEW,0,1,0)
	ELSE
	  WRITE (KLINE$,"('Speed set to ',F8.2,'kph.')") SETSPEED(IUNIT,ISIDE)
	  CALL MSGOUT (IVIEW,0,1,0)
	ENDIF

	IF (SPDU(IUNIT,ISIDE) .GT. 0.0 ) THEN
	  IF (IFLYTYPE .GT. 0 ) THEN
	    WRITE (KLINE$,"('Moving at ',F8.2,'knots')") SPDU(IUNIT,ISIDE) * KM2KNOTS
	    CALL MSGOUT (IVIEW,0,0,0)
	  ELSE
	    WRITE (KLINE$,"('Moving at ',F8.2,'kph')") SPDU(IUNIT,ISIDE)
	    CALL MSGOUT (IVIEW,0,0,0)
	  ENDIF
	ELSE
	  IDEF = IDEFL(IUNIT,ISIDE)

	  IF ( IN_PIT(IUNIT,ISIDE) .GT. 0 ) THEN
	    IF ( IDEF .GE. 0 .AND. IDEF .LE. 4 ) THEN
	      WRITE (KLINE$,"(A18,' in pit ',I3)')") DEFL$(IDEF), IN_PIT(IUNIT,ISIDE)
	      CALL MSGOUT (IVIEW,0,0,0)
	    ENDIF
	  ELSE
	    IF ( IDEF .GE. 0 .AND. IDEF .LE. 4 ) THEN
	      WRITE (KLINE$,"('Stationary ',A18)')") DEFL$(IDEF)
	      CALL MSGOUT (IVIEW,0,0,0)
	    ENDIF
	  ENDIF
	ENDIF

	WRITE (KLINE$,"('Mine Br Mode ',I2)") IBREACH_MINES(IUNIT,ISIDE)
	CALL MSGOUT (IVIEW,0,0,0)

	DELAY  =  TDELAY(IUNIT,ISIDE)
        IF( DELAY .GT. CLOCK )  THEN
	  TIMESTRING$ = '00:00:00:00'
	  CALL CONV_CLOCK( DELAY, TIMESTRING$ )
	  WRITE (KLINE$,"('Delayed until time ',A16)") TIMESTRING$
	  CALL MSGOUT (IVIEW,0,0,0)
	ENDIF

	IF ( ON_BRIDGE(IUNIT,ISIDE) .GT. 0 ) THEN
	  WRITE (KLINE$,"('On bridge ',I5)") ON_BRIDGE(IUNIT,ISIDE)
	  CALL MSGOUT (IVIEW,0,0,0)
	ENDIF

	IF ( IN_BUILDING(IUNIT,ISIDE) .GT. 0 ) THEN
	  IF ( AT_WALL(IUNIT,ISIDE) .GT. 0 ) THEN
	    WRITE (KLINE$,"('Floor ',I2,' of bld ', I4,' in a fire port')")
     *		ON_FLOOR(IUNIT,ISIDE), IN_BUILDING(IUNIT,ISIDE)
	    CALL MSGOUT (IVIEW,0,0,0)
	  ELSE
	    WRITE (KLINE$,"('Floor ',I2,' of bld ', I4)")
     *		ON_FLOOR(IUNIT,ISIDE), IN_BUILDING(IUNIT,ISIDE)
	    CALL MSGOUT (IVIEW,0,0,0)
	  ENDIF
	ELSEIF ( ON_ROOF(IUNIT,ISIDE) .GT. 0 ) THEN
	  WRITE (KLINE$,"('Roof of bld ',I4)") ON_ROOF(IUNIT,ISIDE)
	  CALL MSGOUT (IVIEW,0,0,0)
	ENDIF

	IF ( GOTO_FLOOR(IUNIT,ISIDE) .GT. 0 ) THEN
	  WRITE (KLINE$,"('Moving to floor ',I4)") GOTO_FLOOR(IUNIT,ISIDE) -1
	  CALL MSGOUT (IVIEW,0,0,0)
	ENDIF

	IF (IHOLFIR(IUNIT,ISIDE) .EQ. IWPNHOLD ) THEN
	  WRITE (KLINE$,"('ROE = Weapons Hold  PH min:',F4.2 )")
     *		PHTHRESH(IUNIT,ISIDE)
	  CALL MSGOUT (IVIEW,0,0,0)
	ELSE IF (IHOLFIR(IUNIT,ISIDE) .EQ. IWPNVTIGHT ) THEN
	  WRITE (KLINE$,"('ROE = Weapons V Tight  PH min:',F4.2 )")
     *		PHTHRESH(IUNIT,ISIDE)
	  CALL MSGOUT (IVIEW,0,0,0)
	ELSE IF (IHOLFIR(IUNIT,ISIDE) .EQ. IWPNTIGHT ) THEN
	  WRITE (KLINE$,"('ROE = Weapons Tight  PH min:',F4.2 )")
     *		PHTHRESH(IUNIT,ISIDE)
	  CALL MSGOUT (IVIEW,0,0,0)
	ELSE IF (IHOLFIR(IUNIT,ISIDE) .EQ. IWPNFREE ) THEN
	  WRITE (KLINE$,"('ROE = Weapons Free   PH min:',F4.2 )")
     *		PHTHRESH(IUNIT,ISIDE)
	  CALL MSGOUT (IVIEW,0,0,0)
	ELSE IF (IHOLFIR(IUNIT,ISIDE) .EQ. IWPNVFREE ) THEN
	  WRITE (KLINE$,"('ROE = Weapons V Free   PH min:',F4.2 )")
     *		PHTHRESH(IUNIT,ISIDE)
	  CALL MSGOUT (IVIEW,0,0,0)
	ELSE IF (IHOLFIR(IUNIT,ISIDE) .EQ. IWPNDOWN ) THEN
	  WRITE (KLINE$,"('ROE = Weapons Down   PH min:',F4.2 )")
     *		PHTHRESH(IUNIT,ISIDE)
	  CALL MSGOUT (IVIEW,0,0,0)
	ELSE IF (IHOLFIR(IUNIT,ISIDE) .EQ. IWPNCONCEALED ) THEN
	  WRITE (KLINE$,"('ROE = Weapons concealed   PH min:',F4.2 )")
     *		PHTHRESH(IUNIT,ISIDE)
	  CALL MSGOUT (IVIEW,0,0,0)
	ELSE IF (IHOLFIR(IUNIT,ISIDE) .EQ. IWPNLOOSE ) THEN
	  WRITE (KLINE$,"('ROE = Weapons Loose   PH min:',F4.2 )")
     *		PHTHRESH(IUNIT,ISIDE)
	  CALL MSGOUT (IVIEW,0,0,0)
	ELSE IF (IHOLFIR(IUNIT,ISIDE) .EQ. IWPNVLOOSE ) THEN
	  WRITE (KLINE$,"('ROE = Weapons V Loose   PH min:',F4.2 )")
     *		PHTHRESH(IUNIT,ISIDE)
	  CALL MSGOUT (IVIEW,0,0,0)
	ELSE
	  WRITE (KLINE$,"('ROE = unknown        PH min:',F4.2,x,i2 )")
     *		PHTHRESH(IUNIT,ISIDE), IHOLFIR(IUNIT,ISIDE)
	  CALL MSGOUT (IVIEW,0,0,0)
	ENDIF

	IF( HFWPN(IUNIT,ISIDE) .GT. 0 ) THEN
	  IWPNSLT = HFWPN(IUNIT,ISIDE)
	  IF (IWPNSLT .GT. 0 ) THEN
	    IWPN  =  KWEAPN(IWPNSLT,ITYPE)
	    IF (IWPN .GT. 0 ) THEN
	      WRITE (KLINE$,"('Selected weapon:',A16,X,'PH min:',F4.2 )")
     *			WEAPNAME$(IWPN),
     *			WPNTHRESH(IUNIT,ISIDE,IWPNSLT)
	      CALL MSGOUT (IVIEW,0,0,0)
	    ENDIF
	  ENDIF
	ELSE
	  WRITE (KLINE$,"('Automatic weapon selection enabled.')")
	  CALL MSGOUT (IVIEW,0,0,0)
	ENDIF

	CALL WHAT_SENSOR ( IUNIT, ISIDE, ISENS, ISTYP, IBAND )
	IF ( ISENS .GT. 0 ) THEN
 	  WRITE (KLINE$,"('Sensor:',I2,'/',I2, x, A16)") ISENS, ISTYP, SNSNAME$(ISTYP)
	  CALL MSGOUT (IVIEW,0,0,0)
          IF ( IBAND .EQ. IBAND_SAR1 ) THEN
             ! Using SAR sensor
             ISAR_NUM = ISARNUMBER(IUNIT, ISIDE, ISENS)
             IF (SAR_LIST(ISAR_NUM).GMTI .EQ. .TRUE.) THEN
               GMTI_MSG$ = 'ON'
             ELSE
               GMTI_MSG$ = 'OFF'
             ENDIF

             IF (SAR_LIST(ISAR_NUM).SAR .EQ. .TRUE.) THEN
	       SAR_MSG$ = 'ON'
             ELSE
               SAR_MSG$ = 'OFF'
             ENDIF

             IF (SAR_LIST(ISAR_NUM).FOOTPRINT_MODE .EQ. STRIPMAP) THEN
	       FOOTPRINT_MSG$ = 'STRIPMAP'
             ELSE
	       FOOTPRINT_MSG$ = 'SPOTLIGHT'
             ENDIF

	     WRITE (KLINE$,"('SAR is in ',A,' mode')") FOOTPRINT_MSG$
	     CALL MSGOUT (IVIEW,0,0,0)
             WRITE (KLINE$,"('SAR is ',A3,' |    GMTI is ',A3)") SAR_MSG$, GMTI_MSG$
	     CALL MSGOUT (IVIEW,0,0,0)
	  ELSE ! must be optical or thermal
	    IF ( MOVING_ONLY(ISTYP) .GT. 0) THEN ! special thermal to model GSR
	      WRITE (KLINE$,"('Can only detect targets moving at',F6.1,'kph')") MIN_SNSR_SPEED(ISTYP)
	      CALL MSGOUT (IVIEW,0,0,0)
	    ENDIF
	    IF ( DETECT_AIRCRAFT(ISTYP) .EQ. 1) THEN ! can only detect air targets
	      WRITE (KLINE$,"('Can only detect aircraft targets')")
	      CALL MSGOUT (IVIEW,0,0,0)
	    ENDIF
	    IF ( DETECT_AIRCRAFT(ISTYP) .EQ. 2) THEN ! can only detect gnd targets
	      WRITE (KLINE$,"('Can only detect ground targets')")
	      CALL MSGOUT (IVIEW,0,0,0)
	    ENDIF
	  ENDIF
	ELSE
	  WRITE ( KLINE$,"('Sensors turned off.')")  ! No active sensor
	  CALL MSGOUT (IVIEW,0,0,0)
        ENDIF

	IENGTYP  =  ENGNEER(ITYPE)

	IBRIDGE = BRIDGE_LAYER(ITYPE)

	IF (IBRIDGE .GT. 0 ) THEN
	  IDEP = IAVLBDEP(IUNIT,ISIDE)
	  IF ( IDEP .EQ. IBRIDGE_DEPLOYED) THEN
	    WRITE (KLINE$,"('Bridge deployed')")
	    CALL MSGOUT (IVIEW,0,0,0)
	  ELSEIF ( IDEP .EQ. IBRIDGE_DEPLOYING) THEN
	    WRITE (KLINE$,"('Bridge deploying')")
	    CALL MSGOUT (IVIEW,0,0,0)
	  ELSEIF ( IDEP .EQ. IBRIDGE_RECOVERING) THEN
	    WRITE (KLINE$,"('Bridge recovering')")
	    CALL MSGOUT (IVIEW,0,0,0)
	  ELSEIF ( IDEP .EQ. IBRIDGE_RECOVERED) THEN
	    WRITE (KLINE$,"('Bridge recovered')")
	    CALL MSGOUT (IVIEW,0,0,0)
	  ENDIF

c	  WRITE (KLINE$,"('Bridge carrying ',I3,' vehicles')") BRIDGE_VEHICLES(IUNIT,ISIDE)
c	  CALL MSGOUT (IVIEW,0,0,0)
c	  WRITE (KLINE$,"('       capacity ',I3,' vehicles')") BRIDGE_LIMIT_VEHICLES(IBRIDGE)
c	  CALL MSGOUT (IVIEW,0,0,0)
c	  WRITE (KLINE$,"('Bridge carrying ',I3,' foot')") BRIDGE_FOOT(IUNIT,ISIDE)
c	  CALL MSGOUT (IVIEW,0,0,0)
c	  WRITE (KLINE$,"('       capacity ',I3,' foot')") BRIDGE_LIMIT_FOOT(IBRIDGE)
c	  CALL MSGOUT (IVIEW,0,0,0)

	ENDIF

	IF ( IAM_MOUNTING(IUNIT, ISIDE) .GT. 0 ) THEN
	  IHOST = IAM_MOUNTING(IUNIT,ISIDE)
	  WRITE (KLINE$,"('Attempting to mount unit no ',I5)") IHOST
	  CALL MSGOUT (IVIEW,0,0,0)
	ELSEIF ( MY_CARRIER(IUNIT, ISIDE) .GT. 0 ) THEN
	  IHOST = MY_CARRIER(IUNIT,ISIDE)
	  WRITE (KLINE$,"('Default carrier set to ',I5)") IHOST
	  CALL MSGOUT (IVIEW,0,0,0)
	ELSE
	  WRITE (KLINE$,"('No default carrier defined')")
	  CALL MSGOUT (IVIEW,0,0,0)
	ENDIF

	IRADARTYPE = RADARS(ITYPE)
	IF( IRADARTYPE .GT. 20 ) THEN
	  IF ( KCBRSTATUS(IUNIT,ISIDE) .EQ. KCBRON ) THEN
	    WRITE (KLINE$,"('WLR: setup, on ')")
	    CALL MSGOUT (IVIEW,0,0,0)
	  ELSEIF ( KCBRSTATUS(IUNIT,ISIDE) .EQ. KCBRSETUP ) THEN
	    IF ( CBRTIME(IUNIT,ISIDE) .LE. CLOCK ) THEN
	      WRITE (KLINE$,"('WLR: setup, off ')")
	      CALL MSGOUT (IVIEW,0,0,0)
	    ELSE
	      TIMESTRING$ = '00:00:00:00'
	      CALL CONV_CLOCK( CBRTIME(IUNIT,ISIDE), TIMESTRING$ )
	      WRITE (KLINE$,"('WLR: set up at ',A16)") TIMESTRING$
	      CALL MSGOUT (IVIEW,0,0,0)
	    ENDIF
	  ELSEIF ( KCBRSTATUS(IUNIT,ISIDE) .EQ. KCBRPACKED ) THEN
	    IF ( CBRTIME(IUNIT,ISIDE) .LE. CLOCK ) THEN
	      WRITE (KLINE$,"('WLR: packed up ')")
	      CALL MSGOUT (IVIEW,0,0,0)
	    ELSE
	      TIMESTRING$ = '00:00:00:00'
	      CALL CONV_CLOCK( CBRTIME(IUNIT,ISIDE), TIMESTRING$ )
	      WRITE (KLINE$,"('WLR: packed up at',A16)")  TIMESTRING$
	      CALL MSGOUT (IVIEW,0,0,0)
	    ENDIF
	  ENDIF
	ENDIF

	IF( MY_PASSENGERS(IUNIT,ISIDE) .GT. 0 )  THEN
	  WRITE (KLINE$,"(I3,' Passengers: see pax report')") MY_PASSENGERS(IUNIT,ISIDE)
	  CALL MSGOUT (IVIEW,0,0,0)
	ENDIF

	IF ( MOVERS(ITYPE) .EQ. MOVWHEEL
     *	.OR. MOVERS(ITYPE) .EQ. MOVTRACK
     *	.OR. IFLYTYPE .GT. 0)
     *	THEN
	  IF ( ENGINE_OFF(IUNIT, ISIDE) .GT. 0 ) THEN
	    WRITE (KLINE$,"('Engine is off')")
	    CALL MSGOUT (IVIEW,0,0,0)
	  ELSE
	    WRITE (KLINE$,"('Engine is on')")
	    CALL MSGOUT (IVIEW,0,0,0)
	  ENDIF
	ENDIF

	IF( CMDWPN(IUNIT,ISIDE) .GT. 0 ) THEN
	  IWPNSLT = CMDWPN(IUNIT,ISIDE)
	  IF (IWPNSLT .GT. 0 ) THEN
	    IWPN  =  KWEAPN(IWPNSLT,ITYPE)
	    IF (IWPN .GT. 0 ) THEN
	      WRITE (KLINE$,"('Command weapon:',A16 )")
     *			WEAPNAME$(IWPN)
	      CALL MSGOUT (IVIEW,0,0,0)
	    ENDIF
	  ENDIF
	ELSE
	  WRITE (KLINE$,"('Command weapon: none selected')")
	  CALL MSGOUT (IVIEW,0,0,0)
	ENDIF

	IF( AREAWPN(IUNIT,ISIDE) .GT. 0 ) THEN
	  IWPNSLT = AREAWPN(IUNIT,ISIDE)
	  IF (IWPNSLT .GT. 0 ) THEN
	    IWPN  =  KWEAPN(IWPNSLT,ITYPE)
	    IF (IWPN .GT. 0 ) THEN
	      WRITE (KLINE$,"('Area weapon:',A16 )")
     *			WEAPNAME$(IWPN)
	      CALL MSGOUT (IVIEW,0,0,0)
	    ENDIF
	  ENDIF
	ELSE
	  WRITE (KLINE$,"('Area weapon: none selected')")
	  CALL MSGOUT (IVIEW,0,0,0)
	ENDIF

	IF (IAMLEADING(IUNIT,ISIDE) .EQ. IFMODEFORM ) THEN
	  WRITE (KLINE$,"('Leading a formation')")
	  CALL MSGOUT (IVIEW,0,0,0)
	ELSEIF (IAMLEADING(IUNIT,ISIDE) .EQ. IFMODECONV ) THEN
	  WRITE (KLINE$,"('Leading a convoy')")
	  CALL MSGOUT (IVIEW,0,0,0)
	ENDIF
	IF (IAMFOLLOWING(IUNIT,ISIDE) .GT. 0 ) THEN
	  JUNIT = IAMFOLLOWING(IUNIT,ISIDE)
	  JTYPE  =  KSYSTYP(JUNIT,ISIDE)
	  IF (JTYPE .GT. 0 ) THEN
	    CALL TRRNLOC ( XUNIT(JUNIT,ISIDE), YUNIT(JUNIT,ISIDE),
     *					IXM, IYM)
	    WRITE (KLINE$,"('In fmn with ',I4,X,A16 )") JUNIT, SYSTNAME$(JTYPE)
	    CALL MSGOUT (IVIEW,0,0,0)
	  ELSE
	    TYPE *,'WARNING - unit/side is following an invalid icon.',
     *			IUNIT, ISIDE
	  ENDIF
	ENDIF

	IF (IAMCONVOYING(IUNIT,ISIDE) .GT. 0 ) THEN
	  JUNIT = IAMCONVOYING(IUNIT,ISIDE)
	  JTYPE  =  KSYSTYP(JUNIT,ISIDE)
	  IF (JTYPE .GT. 0 ) THEN
	    CALL TRRNLOC ( XUNIT(JUNIT,ISIDE), YUNIT(JUNIT,ISIDE),
     *					IXM, IYM)
	    WRITE (KLINE$,"('In convoy with ',I4,X,A16 )") JUNIT, SYSTNAME$(JTYPE)
	    CALL MSGOUT (IVIEW,0,0,0)
	  ELSE
	    TYPE *,'WARNING - unit/side is following an invalid icon.',
     *			IUNIT, ISIDE
	  ENDIF
	ENDIF
		 */
		
		return s;
	}
	
}
