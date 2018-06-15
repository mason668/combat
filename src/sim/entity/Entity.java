package sim.entity;

import java.util.Vector;

import data.csd.Platform;
import data.csd.PlatformWeapon;
import data.csd.Sensor;
import data.csd.Weapon;
import data.managers.Identifiable;
import data.map.Building;
import data.map.Coordinate;
import sim.Constants;
import sim.FirePort;
import sim.obstacles.Pit;
import sim.forces.Force;
import sim.obstacles.Obstacle;
import sim.route.Node;
import sim.route.Route;
import utils.Logger;

public class Entity implements ObserverEntity, 
	FirerEntity, 
	MoverEntity, 
	TargetEntity, 
	BridgeEntity, 
	CarrierEntity, 
	DetectedEntity
	{
	
//	private String movementModel = "models.movement.MoveEvent";
//	private String movementModel = "models.janus.JanusMoveEvent";
	private String movementModel = "";
	public String getMovementModel(){
		return movementModel.substring(0);
	}
	//TODO need to be able to set model
	//TODO add other model names
	
	private Platform myPlatform; // globunits.ksystyp, kcsdtyp
	public Platform getPlatform(){return myPlatform;}
	
	public Entity (String s, Platform p){
		name = s.substring(0);
		myPlatform = p;
		for (PlatformWeapon slot: myPlatform.getWeaponList()){
			EntityWeapon weapon = new EntityWeapon(slot);
			weaponList.addElement(weapon);
		}
		if (this.getPlatform().getSensorList().getSize()>0){
			this.setCurrentSensor(this.getPlatform().getSensorList().getFirst());
		}
	}
	
	private Vector<EntityListener> entityListeners = new Vector<EntityListener>();
	public void addEntityListener(EntityListener listener){
		if (listener == null) return;
		entityListeners.add(listener);
	}

	/*
	 * Turn on and off tracing within the simulation models 
	 */
	private boolean trace = false; // globunits.itrack
	public boolean isTracing(){return trace;}
	public void setTracing(boolean b){
		trace = b;
	}

	private String name = "test_entity";
	public String getName(){return name.substring(0);}
	public void setName(String name) {
		this.name = name.substring(0);
	}
	
	private String callSign = ""; // globunits.unitid
	public String getID(){return getName();}
	
	// data from globunits
	
	private int currentElements=1; // globunits.nscore
	public int getNumberOfElements(){return currentElements;}
	public void setNumberOfElements(int i){currentElements = i;}
	private int orginalElements = 1; // globunits.knumele

	private int currentPersonnel = 1; // globunits.perscur
	private int originalPersonnel = 1; // globunits.perstot
	
	private int sopOnContact = 0; // globunits.on_contact
	private int sopOnEngaged = 0; // globunits.on_engaged
	private int sopMine = 0; // globunits.on_mine
	private int[] sopTrigger = new int[Constants.MAX_TRIGGERS]; // globunits.on_trigger
	private int[] sopObstacle = new int[Constants.MAX_OBSTACLE_TYPES]; // globunits.on_obstacle
	private String[] sopTriggerCommand = new String[Constants.MAX_TRIGGERS]; // globunits.on_trigger_command
	
	private int nonMovers = 0; // globunits.nonmovers
	public void setNonMovers(int i){
		if ( i < currentElements ){  // TODO should this be initialElements?
			nonMovers = i;
		} else {
			nonMovers = currentElements;
		}
	}
	public int getNonMovers(){return nonMovers;}
	public int getMovers(){return this.currentElements - this.nonMovers;}
	
	private int nonFirers = 0; // globunits.nonfirers
	public int getNonFirers(){return this.nonFirers;}
	public int getFirers(){return this.currentElements - this.nonFirers;}

	private int[] currentCasualties = new int[Constants.MAX_CASUALTY_TYPES]; // globunits.kastot
	
	private Coordinate currentLocation = new Coordinate( 1.0, 1.0, 0.0); // globunits.xunit, yunit, zunit
	public void setLocation(Coordinate c){
		//update the entity's location
		currentLocation = new Coordinate(c);
		// let any listeners know the entity has moved
		for (EntityListener listener: entityListeners){
			listener.updateLocation(this, c);
		}
		/* TODO do we do this here? from set_xy
		 * 
	  IF ( CLOCK_TICKING ) THEN
	    IF ( DOEVENTQUE .GT. 0 ) THEN
	    CALL ALTER_X(IUNIT,ISIDE)
	    ENDIF
		 */
	}
	public Coordinate getLocation(){
		return new Coordinate(currentLocation);
	}
	public void setX(double x){
		currentLocation.setX(x);
	}
	public void setY(double y){
		currentLocation.setY(y);
	}
	
	private double timeToMove = 0; // globunits.t2move
	private double timeToRadar = 0; // globunits.t2radar
	private double timeTofire = 0; // globunits.t2fire
	private double timeToCas = 0; // globunits.t2cas
	private double timeToSuppress = 0; // globunits.t2supr
	private double timeToUpload = 0; // globunits.t2upload
	private double timeToResupply = 0; // globunits.t2resupply
	private double timeToSearch = 0; // globunits.t2search TODO is this used?
	private double timeToFriends = 0; // globunits.t2friends TODO not used?
	private double timeToDetectObstacles = 0; // globunits.t2obs
	private double timeToScan = 0; // globunits.t2scan, t2scanf
	private double timeToADRadar = 0; // globunits.tiadrdar
	private double firnxt = 0; // globunits.firnxt TODO I doubt this is used
	
	private int checkFire = 0; // globunits.check_fire TODO could be boolean?
	
	private double phThreshold = 0.5; // globunits.phthresh

	/* TODO top of globunits
C------ FILENAME:  GLOBUNITS.FOR ------------------------ A.D.KELLNER, TRAC-WSMR
	REAL	WPNTHRESH(NUMUNITS,NUMSIDES,NWPNSYS)!
						! ph threshold for each wpn on
						! icon
        COMMON / WPNTHRESH / WPNTHRESH

	INTEGER*2	KANSEE(NUMUNITS,NUMSIDES,NMVISB,NUMSIDES,2)!NUMUNITS,NUMSIDES,NUMUNITS,NUMSIDES,2)
						! Which enemy units are seen
        COMMON / KANSEE / KANSEE

	INTEGER*2	KDETLVL(NUMUNITS,NUMSIDES,MAXFORCES)	! What observation
							! level am I at on each
							! station
        COMMON / KDETLVL / KDETLVL

	INTEGER*2	KSEESME(NUMUNITS,NUMSIDES,MAXFORCES)	! Who sees me at above
							! level
        COMMON / KSEESME / KSEESME

	BYTE	ISEENMY(NUMUNITS,NUMSIDES)	! target seen
        COMMON / ISEENMY / ISEENMY

	INTEGER*2	ISEETRGTS(NUMUNITS,NUMSIDES,NUMSIDES)	! how many
						! targets are seen on each side
						! by each unit
        COMMON / ISEETRGTS / ISEETRGTS
*/
	private Sensor currentSensor = null; // globunits.isensno
	public Sensor getCurrentSensor(){return currentSensor;}
	public void setCurrentSensor(Sensor sensor){
		currentSensor = sensor;
	}//TODO validate - does the entity have this sensor
	private Sensor lastSensor = null;
	public Sensor getLastSensor(){
		return lastSensor;
	}
	public void setLastSensor (Sensor sensor){
		lastSensor = sensor; //TODO validate entity has this sensor
	}
	
/*
	BYTE	IWPNNO(NUMUNITS,NUMSIDES)	! Which weapon to use. If 0
						! then use all weapons.
        COMMON / IWPNNO / IWPNNO
        */
	/**
	 * A flag to indicate the entity is currently firing
	 */
	private boolean iAmFiring = false; // globunits.iamfirng
	private double currentFuel = 100; // globunits.fuel
	public double getCurrentFuel(){return currentFuel;}
	public void setCurrentFuel(double d){currentFuel = d;}
	private boolean engineOn = false; // globunits.engine_off
	public boolean isEngineOn(){return engineOn;}
	public void setEngineOn(){engineOn = true;}
	public void setEngineOff(){engineOn = false;}
	
	private double lastFiredTime = 0; // globunits.flast
	public double getLastFired(){
		return lastFiredTime;
	}
	public void setLastFire(double time){
		lastFiredTime = time;
	}
	//TODO last fired by weapon and force
//	REAL	FLAST_WPN  (NUMUNITS,NUMSIDES,NWPNSYS)
//	REAL	FLAST_FORCE (NUMUNITS, NUMSIDES, MAXFORCES)
	private double suppressionAmount = 0.0; // globunits.tsuprs
	public double getSuppressionAmount(){return suppressionAmount;}
	public void setSuppressionAmount(double d){suppressionAmount = d;}
	public void addSuppression(double d){suppressionAmount+= d;}
	public boolean isSuppressed(){
		if ( Math.random()<=this.suppressionAmount){
			return true;
		}
		return false;
	}
	
	private double lastSuppressedTime = 0; // globunits.last_suppressed
	private double currentSpeed = 0.0; // globunits.spdu
	public double getCurrentSpeed(){return currentSpeed;}
	public void setCurrentSpeed(double d){ currentSpeed = d;}
	private boolean crawlMode = false; // globunits.icrawl
	public boolean iAmCrawling(){return crawlMode;}
	
        /*

	REAL	MYSIZE(NUMUNITS,NUMSIDES)
	REAL	MYREALSIZE(NUMUNITS,NUMSIDES)
	INTEGER*4	MYCONCLASS(NUMUNITS,NUMSIDES)
	REAL	MYHEIGHT(NUMUNITS,NUMSIDES)	! height for the purpose of being detected
	COMMON / MYSIZE / MYSIZE, MYREALSIZE, MYCONCLASS, MYHEIGHT
	
	*/
	private double altitude = 0; // globunits.altitude should always be zero for non fliers
	public double getAltitude(){return altitude;}
	public void setAltitude(double d){altitude = d;}
	private boolean landed = true; // globunits.landed
	private int defiladeState = Constants.DEFILADE_EXPOSED; // globunits.idefl
	public int getDefilade(){return defiladeState;}
	public void setDefilade (int i){defiladeState = i;}// TODO validate
	private int roeState = Constants.ROE_TIGHT; // globunits.iholfir
	public int getROEState(){return roeState;}
	public void setROEState(int i){roeState = i;}//TODO validate ROE
	
	
	
	/*

	INTEGER*4	ICON_MIN_PRIORITY(NUMUNITS, NUMSIDES)
	INTEGER*4	ICON_VS_ENEMY(NUMUNITS,NUMSIDES)
	INTEGER*4	ICON_VS_NEUTRAL(NUMUNITS,NUMSIDES)
	INTEGER*4	ICON_VS_UNKNOWN(NUMUNITS,NUMSIDES)

	INTEGER*4	ICON_RANGE_ENEMY(NUMUNITS, NUMSIDES)
	INTEGER*4	ICON_RANGE_NEUTRAL(NUMUNITS,NUMSIDES)
	INTEGER*4	ICON_RANGE_UNKNOWN(NUMUNITS,NUMSIDES)

	COMMON / ROE_STATUS2 /
     *		ICON_MIN_PRIORITY,
     *		ICON_VS_ENEMY,
     *		ICON_VS_NEUTRAL,
     *		ICON_VS_UNKNOWN,
     *		ICON_RANGE_ENEMY,
     *		ICON_RANGE_NEUTRAL,
     *		ICON_RANGE_UNKNOWN
     *
     **/

	private double lastMoved = -Constants.NEVER; // globunits.tmoved
	public double getLastMoved(){return lastMoved;}
	public void setLastMoved(double d){
		if ( d> lastMoved) lastMoved = d;
	}
	private int slowToShoot = 0; // globunits.islow
	public void setSlowToShoot(boolean b){} //TODO tell then entity to slow when moving or not
	
	private double delay = 0.0;// globunits.tdelat this entity is delayed until this time
	public double getDelay(){return delay;}
	public void setDelay(double d){delay = Math.max(d,delay);} // set the delay time - includes clock - need to make sure it does not get reduced
	public void clearDelay(){delay = 0.0;} // needed for admin to clear delay
	//public void addDelay(double time){} // TODO add the supplied time to the current clock and delay until then

	private Force myForce = null; // globunits.myforce
	public Force getForce(){return myForce;}
	public void setForce (Force force){myForce = force;} //TODO do we add to force here?
	private int myGroup= 1; // globunits.mygroup
	
	private int inoperative = 0; // globunits.kinopstat chem/ nbc state
	public int getNBCInoperative(){return inoperative;}
	public void setNBCInoperative(int state){inoperative = state;} //TODO validate zero is ok = off
	
	/*
	 * 

	BYTE	NTFDEV (NUMUNITS,NUMSIDES)	! Unit's logical view
        COMMON / NTFDEV / NTFDEV

	BYTE	MYBDE  (NUMUNITS,NUMSIDES)	! Unit's logical brigade
        COMMON / MYBDE / MYBDE
        
        */
	
	private boolean mopp = false;
    public boolean getMOPP(){ return mopp;}
    public void setMOPP(boolean b){mopp = b;}

    private CarrierEntity currentCarrier; // gobunits.mounted
	public boolean isMounted(){return (currentCarrier != null);}
	public CarrierEntity getCarrier(){return currentCarrier;}
//TODO not needed - see mount	public void setCarrier(Entity entity){currentCarrier = entity;}a
	
	private int mountedElements = 0; // globunits.mountele
	private int numberOfPassengers = 0; // globunits.mypassengers
	private CarrierEntity defaultCarrier; // globunits.my_carrier
	public void SetDefaultCarrier(CarrierEntity carrier){
		defaultCarrier = carrier;
	}
	private boolean iamMounting = false; // globunits.iam_mounting TODO may not be used

	public FirePort atFirePort(){return null;} // globunits.at_wall TODO should return a pointer to a wall
	public void clearFirePort(){} // TODO should redraw icon
	private double atWallAngle = 0; // globunits.at_wall_angle
	
	private Building inBuilding = null; // globunits.in_building
	public Building getBuilding(){return inBuilding;}
	public void setBuilding(Building b){inBuilding = b;}

	private Building onRoof = null; // globunits.on_roof
	public Building getRoof(){return onRoof;}
	public void setRoof(Building b){onRoof = b;}
	/*
	 * Floors start counting from zero - ie ground floor.
	 */
	private int onFloor = -1; // globunits.on_floor
	public int getFloor(){return onFloor;}
	public void setFloor(int i){onFloor = i;}
	private int gotoFloor = -1;
	public int getGotoFloor(){return gotoFloor;}
	public void setGotoFloor(int i){gotoFloor = i;}
	public void clearGotoFloor(){
		gotoFloor = -1;
	}

	private BridgeEntity onBridge = null; // globunits.on_bridge
	public BridgeEntity onBridge(){return onBridge;}//TODO point to bridge entity
	public boolean enterBridge(BridgeEntity bridge){
		onBridge = bridge;
		return true;
	}
	private int vehiclesOnBridge = 0; // globunits.bridge_vehicles
	private int footOnBridge = 0; // globunits.bridge_foot

	/*

	INTEGER*4	KRLEFT (NWPNSYS,NUMUNITS,NUMSIDES)  ! Rounds of basic load
                                                     !  remaining
	INTEGER*4	KSLEFT (NWPNSYS,NUMUNITS,NUMSIDES) ! rounds left in stored bin

	INTEGER*2	AMMO_PERCENT(NUMUNITS, NUMSIDES) ! ammount of ammo as % to start with
        COMMON / KRLEFT / KRLEFT, AMMO_PERCENT, KSLEFT

	INTEGER*2	LOGWPN(NUMUNITS, NUMSIDES) ! when resupplying ammo, use all weapons (zero) or a specific weapon
	COMMON / LOGWPN / LOGWPN

	INTEGER*4	KWLEFT(NUMUNITS,NUMSIDES)           ! Carrying Weight Left.
                                                     ! (lbs)
        COMMON / KWLEFT / KWLEFT

	INTEGER*4	KVLEFT(NUMUNITS,NUMSIDES)           ! Carrying Volume Left.
                                                     ! (Cu Ft)
        COMMON / KVLEFT / KVLEFT
        
    */

	private int mineClearingMode = Constants.MINE_CLEAR_BULL; // globunits.kmclrfun - TODO is this the same?
	public int getMineClearingMode(){return mineClearingMode;}
	private int mineClearingDevice = 0; // globunits.kmclrfun: 1 = plough, 2 = roler, 3 = line
	private int mine_clearingDevices = 1; // globunits.kmclrlod
	private int mineLoad = 0; // globunits.kminlod

	
	/*

	INTEGER*4	MINE_LOAD(NUMUNITS,NUMSIDES,3) ! why are there 3?
        COMMON / KMINLOD / KMINLOD, MINE_LOAD

	INTEGER*4	IDISPLAY_CALLSIGN(NUMUNITS,NUMSIDES)
        COMMON / UNITID$ / UNITID$, IDISPLAY_CALLSIGN

	REAL	RLDWPN(NWPNSYS,NUMUNITS,NUMSIDES) ! Time when each weapon will
						! be reloaded, or was last
						! loaded.
        COMMON / RLDWPN / RLDWPN

	INTEGER*4	IWPN_RATE(NWPNSYS, NUMUNITS, NUMSIDES) ! which rate to use

	COMMON / WPN_RATE/ IWPN_RATE

	INTEGER*4	POINT_DEFENCE_ROUNDS(NUMUNITS,NUMSIDES)
	INTEGER*4	POINT_DEFENCE_STORED(NUMUNITS,NUMSIDES)
	INTEGER*4	ARMOUR_PANELS(NUMUNITS,NUMSIDES)

	COMMON / POINT_DEFENCE /
     *		POINT_DEFENCE_ROUNDS,
     *		POINT_DEFENCE_STORED,
     *		ARMOUR_PANELS

	REAL	UNITRAD(NUMUNITS,NUMSIDES)	! The radius on the ground
						! of the area occupied by this
						! unit.
        COMMON / UNITRAD / UNITRAD

	BYTE	ICASFCT(NUMUNITS,NUMSIDES)	! The % of casualties to make
						! unit turn yellow.
        COMMON / ICASFCT / ICASFCT

	BYTE	IAMOFCT(NUMUNITS,NUMSIDES)	! The % of ammo used to make
						! unit turn yellow.
        COMMON / IAMOFCT / IAMOFCT

	REAL	LAST_TGT_X(NUMUNITS,NUMSIDES)	! The x co-ord for last target
        COMMON / LAST_TGT_X / LAST_TGT_X

	REAL	LAST_TGT_Y(NUMUNITS,NUMSIDES)
        COMMON / LAST_TGT_Y / LAST_TGT_Y

	REAL	LAST_TGT_T(NUMUNITS,NUMSIDES)	! the time fired at last target (mins)
        COMMON / LAST_TGT_T / LAST_TGT_T

	REAL	PRI_TGT_X(NUMUNITS,NUMSIDES)	! The x coord fr FPF target
        COMMON / PRI_TGT_X / PRI_TGT_X

	REAL	PRI_TGT_Y(NUMUNITS,NUMSIDES)
        COMMON / PRI_TGT_Y / PRI_TGT_Y

	INTEGER*4	PRI_TGT_ID(NUMUNITS,NUMSIDES)	! ID of target, prepos number
						! if firer is arty.
        COMMON / PRI_TGT_ID / PRI_TGT_ID

	REAL	FOOT_PRINT(NUMUNITS,NUMSIDES)	! the foot print of this icon
						! in KM
        COMMON / FOOT_PRINT / FOOT_PRINT

	INTEGER*4	ISARNUMBER(NUMUNITS,NUMSIDES,NUMSENS)
						! If this icon is a SAR, then
						! this specifies the particular
						! instance. Points to SAR_LIST
        COMMON / ISARNUMBER / ISARNUMBER

	INTEGER*2	ROUTE_LOCK(NUMUNITS,NUMSIDES)	! Switch to determine if this
						! icon has its route locked.
						! Should default to 0 ie not
						! locked.
        COMMON / ROUTE_LOCK / ROUTE_LOCK

	INTEGER*2	ICON_SELECTED(NUMUNITS,NUMSIDES,NUMSTAT) ! 1 if this icon
						! has been selected on the
						! current view. See jan_witchmm
						! and funcrot. Used for scope
						! commands
        COMMON / ICON_SELECTED / ICON_SELECTED

	INTEGER*2	JANUS_CONTROL(NUMUNITS,NUMSIDES) ! determines if this entity is
						! controlled by Janus or not
        COMMON / JANUS_CONTROL / JANUS_CONTROL

C-------------------------------------------------------------------------------


	REAL		ARRAY_X_REAL(NUMUNITS * NUMSIDES)
	INTEGER*4	ARRAY_X_INT(NUMUNITS*NUMSIDES)
	INTEGER*4	ARRAY_X_INDEX(NUMUNITS*NUMSIDES)
	INTEGER*4	IELEMENTS_X

	COMMON / SORTED_ARRAYS /
     *		ARRAY_X_REAL,
     *		ARRAY_X_INT,
     *		ARRAY_X_INDEX,
     *		IELEMENTS_X

	INTEGER*4	MAXOVRLINES
	PARAMETER	(MAXOVRLINES = 100)

	INTEGER*4	NUMOVRLINES
	INTEGER*4	LINE_FORCE(MAXOVRLINES)
	REAL*4		LINEX(MAXOVRLINES,2)
	REAL*4		LINEY(MAXOVRLINES,2)
	INTEGER*4	LINE_APPLIES(MAXOVRLINES,MAXFORCES)
	INTEGER*4	LINE_OVERLAY(MAXOVRLINES)

	COMMON / LINES/
     *		NUMOVRLINES,
     *		LINE_FORCE, LINEX, LINEY, LINE_APPLIES, LINE_OVERLAY

	INTEGER*4	FORCE_FIRST_ICON(NUMFORCES, NUMSIDES)
	INTEGER*4	FORCE_NEXT_ICON(NUMUNITS, NUMSIDES)
	INTEGER*4	FORCE_PREV_ICON(NUMUNITS, NUMSIDES)

	COMMON / FORCE_LIST /
     *		FORCE_FIRST_ICON, FORCE_NEXT_ICON, FORCE_PREV_ICON


c	INTEGER*4	NUMFORCES, NUMGROUPS, MAXFORCES, MAXGROUPS
c	PARAMETER ( NUMFORCES = NUMSTAT )
c	PARAMETER ( NUMGROUPS = NGPSPERFRC * NUMFORCES )
c	PARAMETER ( MAXFORCES = NUMSIDES*NUMFORCES )
c	PARAMETER ( MAXGROUPS = NGPSPERFRC * MAXFORCES)


	INTEGER*4	FORM_FIRST_ICON(NUMUNITS, NUMSIDES)
	INTEGER*4	FORM_NEXT_ICON(NUMUNITS, NUMSIDES)
	INTEGER*4	FORM_PREV_ICON(NUMUNITS, NUMSIDES)

	COMMON / FORM_LIST /
     *		FORM_FIRST_ICON, FORM_NEXT_ICON, FORM_PREV_ICON


	 * 
	 */
	
	// pointer into the xarray structure inside the entity list. Maybe this should be external to the entity object
	private int xarrayPointer = -1;
	public int getXarrayPointer(){return xarrayPointer;}
	public void setXarrayPointer(int i){xarrayPointer = i;}
	
	public boolean isDead(){
		if (currentElements <=0) return true;
		if (this.inoperative>0) return true;
		return false;
	}
	
	private CarrierEntity commandCarrier;
	public CarrierEntity getCommandMount(){
		return commandCarrier;
	}
	public void setCommandMount(CarrierEntity carrier){
		commandCarrier = carrier; //TODO validate etc
	}

	public void mount (){
		mount(this.defaultCarrier);
	}
	
	public void mount(CarrierEntity carrier){
		// actually do the mount
		//TODO lots of validating etc
		if (carrier != null){
			currentCarrier = carrier;
			commandCarrier = null;
		}
	}
	
	private Pit inPit;
	private Pit commandPit;
	public Pit getPit(){return inPit;}
	public Pit getCommandPit(){return commandPit;}
	public void setCommandPit(Pit pit){
		this.commandPit = pit;
	}
	public void occupyPit(Pit pit){
		inPit = pit;
		pit.setOccupant(this);
	}
	
	private Vector<EntityWeapon> weaponList = new Vector<EntityWeapon>();
	public Vector<EntityWeapon> getWeaponList(){return weaponList;}
	public EntityWeapon getEntityWeapon(Weapon weapon){
		for (EntityWeapon e: weaponList){
			if (e.getWeapon() == weapon) return e;
		}
		return null;
	}
	public EntityWeapon getEntityWeapon(PlatformWeapon platformWeapon){
		for (EntityWeapon e: weaponList){
			if (e.getWeaponSlot() == platformWeapon) return e;
		}
		return null;
	}

	private boolean conicalView = false;
	public boolean isConicalView(){return conicalView;}
	public void updateConicalView(){ //TODO conical view
		/*
		 * from movflyer
		 * 
		 * 
	TODO something about conical view - who knows...
C  this ought to continue working when offset viewing is used, providing
C  %CONIC is set when first accessed in VIEWFAN(?).

	IF (CONICVIEW(IUNIT,ISIDE))THEN
	  SPOTLNG(IUNIT,ISIDE) = SQRT(DXC**2+DYC**2+DZC**2)
	  SLEN = SPOTLNG(IUNIT,ISIDE)
	  GRDLNG = SQRT(DXC**2+DYC**2)
	  IF (IVIEWDIR(IUNIT,ISIDE) .NE. IPTVIEW)THEN
	    ANGLE = DVIEW(IUNIT,ISIDE)
	    SANGLE = SIN(ANGLE)
	    CANGLE = COS(ANGLE)
	    DXVIEW = GRDLNG * CANGLE
	    DYVIEW = GRDLNG * SANGLE
	    XCONIC(IUNIT,ISIDE)  =  XUNIT(IUNIT,ISIDE) + DXVIEW
	    YCONIC(IUNIT,ISIDE)  =  YUNIT(IUNIT,ISIDE) + DYVIEW
	    ZCONIC(IUNIT,ISIDE)  =  ALTUDE(XCONIC(IUNIT,ISIDE),
     *		YCONIC(IUNIT,ISIDE))*0.001
	  ELSE
	    IF (GRDLNG .EQ. 0.0) THEN
	      IVIEW = 0
cPhoenix	      CALL HIUNIT ( IUNIT, ISIDE, IVIEW, L )
c	      WRITE (L,10002)
10002	      FORMAT (X,'000 Zero ground length')
	    ENDIF
	    ALPHA = ATAN2(GRDLNG, ZO)
	    CONEANG = DLEFT(IUNIT,ISIDE)
	    ALPHA2 = ALPHA + CONEANG
	    RANGE = (ZO * TAN(ALPHA2)) - GRDLNG
	    RCONECIRC(IUNIT,ISIDE) = RANGE
	  ENDIF
	  CALL WRITCONIC(IUNIT,ISIDE)
	ENDIF
		 * 
		 */
	}

	private Vector<EntityDetection> detectionList = new Vector<EntityDetection>();
	public Vector<EntityDetection> getDetectionList(){return detectionList;}
	public void updateDetection(Entity target, int level){
		if (level < Constants.OBSERVATION_LEVEL_UNSEEN) return;
		if (level > Constants.OBSERVATION_LEVEL_IDENTIFIED) return;
		if (target == null) return;
		boolean done = false;
		loop:
		for (EntityDetection e: detectionList){
			if (e.getEntity() == target){
				if (level == Constants.OBSERVATION_LEVEL_UNSEEN){
					detectionList.remove(e);
				} else {
					e.setObservationLevel(level);
				}
				done = true;
				break loop;
			}
		}
		if (!done){
			if (level == Constants.OBSERVATION_LEVEL_UNSEEN) return;
			EntityDetection e = new EntityDetection(target, level);
			detectionList.addElement(e);
		}
	}
	public int getDetectionLevel(DetectedEntity target){ // TODO is this the quickest way?
		for (EntityDetection e: detectionList){
			if (e.getEntity() == target){
				return e.getObservationLevel();
			}
		}
		return Constants.OBSERVATION_LEVEL_UNSEEN;
	}
	private Entity spotTarget = null; // has this entity been ordered to try to spot a specific enemy?
	public Entity getSpotTarget(){ //TODO make spotting work
		return spotTarget;
	}
	private TargetList targetList = new TargetList(); //TODO should it start as null?
	public TargetList getTargetList(){
		return targetList;
	}
	public void setTargetList(TargetList  list) {
		targetList = list;
	}
	
	public int getVulnerability(){return 1;}//TODO
	public Vector<Coordinate> getElementLocations(){
		Vector<Coordinate> elementLocations = new Vector<Coordinate>();
		int elements = getNumberOfElements();
		for (int i=0; i<elements; i++){
			//TODO
			//  CALL ELEMLOC ( JUNIT, JSIDE, XARRAY, YARRAY, ZARRAY )
			elementLocations.add(new Coordinate(currentLocation));
		}
		return elementLocations;
	}

	private double directionMove = 2.0 * Math.PI * Math.random();
	public double getDirectionMove(){return directionMove;}
	public void setDirectionMove(double direction){
		directionMove = direction;
		while (this.directionMove > Constants.TWO_PI) {this.directionMove -= Constants.TWO_PI;}
		while (this.directionMove < -Constants.TWO_PI) {this.directionMove += Constants.TWO_PI;}
	}
	private double directionFace = directionMove;
	public double getDirectionFace(){return directionFace;}
	public void updateDirectionFace(){
		updateDirectionFace(this.directionMove);
	}
	public void updateDirectionFace(double direction){
		this.directionFace = direction;
		if (this.moveMode == Constants.MOVE_REVERSE){
			this.directionFace += Math.PI;
		}
		while (this.directionFace > Constants.TWO_PI) {this.directionFace -= Constants.TWO_PI;}
		while (this.directionFace < -Constants.TWO_PI) {this.directionFace += Constants.TWO_PI;}
	}
	
	private double directionOffset = 0.0;
	private Coordinate viewSpot = null;
	private double directionView = directionMove;
	public double getDirectionView(){return directionView;};
	public void updateDirectionView(){
		updateDirectionView(this.directionMove);
	}
	public void updateDirectionView(double direction){
		if (this.getViewMode() == Constants.VIEW_POINT) {
			if (this.viewSpot == null){
				this.setViewMode(Constants.VIEW_NORMAL);
			} else {
				this.directionView = Coordinate.direction(this.currentLocation, 
						this.viewSpot); //TODO need to confirm right way around
			}
		} else {
			if (this.getViewMode() == Constants.VIEW_OFFSET){
				directionView = direction + this.directionOffset;
			} else if (this.getViewMode() == Constants.VIEW_POINT) {
				if (this.viewSpot == null){
					this.setViewMode(Constants.VIEW_NORMAL);
				} else {
					this.directionView = Coordinate.direction(this.currentLocation, 
							this.viewSpot); //TODO need to confirm right way around
				}
			} else {
				this.directionView = direction;
			}
			if (this.moveMode == Constants.MOVE_REVERSE){
				this.directionView += Math.PI;
			}
		}
		while (this.directionView > Constants.TWO_PI) {this.directionView -= Constants.TWO_PI;}
		while (this.directionView < -Constants.TWO_PI) {this.directionView += Constants.TWO_PI;}
	}
	
	private double commandSpeed = 100.0;
	public double getCommandSpeed(){return commandSpeed;}
	public void setCommandSpeed(double d){
		commandSpeed = d;
		/*
	SUBROUTINE  SET_SPEED ( INUNIT, ISIDE, SPEED)

	include 	'global.f'
	include 	'globunits.f'
	include 	'globbut.f'
	include		'globmenu.f'
	include		'globrpt.f'
	include		'globmove.f'
	include		'glbflyer.f'

	IUNIT = INUNIT

	call validunit ( iunit, iside)
	if ( iunit .le. 0 ) return

C------ Change SPRINT mode of picked unit

	ISCORE = NSCORE(IUNIT,ISIDE)
	IF ( ISCORE .LE. 0 ) THEN
	  GOTO 999
	ENDIF

	IF ((ROUTE_LOCK(IUNIT,ISIDE) .EQ. ION ) .AND.
     *		(DO_ROUTE_LOCKS .EQ. ION ))THEN
	  GOTO 999
	ENDIF

	ITYPE     =  KSYSTYP(IUNIT,ISIDE)
	IFLYTYPE = FLYERS(ITYPE)

	ICRAWL(IUNIT,ISIDE) = 0
	KSPRINT(IUNIT,ISIDE)  =  MOVSLOW
	SETSPEED(IUNIT,ISIDE) = SPEED

	IF (IFLYTYPE .GT. 0)THEN
	  IF (SETSPEED(IUNIT,ISIDE) .GT. FLYVELOC(MAXINDEX,IFLYTYPE,ISIDE))THEN
	    SETSPEED(IUNIT,ISIDE) = FLYVELOC(MAXINDEX,IFLYTYPE,ISIDE)
	  ELSEIF (SETSPEED(IUNIT,ISIDE) .LT. FLYVELOC(MININDEX,IFLYTYPE,ISIDE)) THEN
	    SETSPEED(IUNIT,ISIDE) = FLYVELOC(MININDEX,IFLYTYPE,ISIDE)
	  ENDIF
	ELSE
	  IF (SETSPEED(IUNIT,ISIDE) .GT. ISPDT(ITYPE)) THEN
	    SETSPEED(IUNIT,ISIDE) = ISPDT(ITYPE)
	  ENDIF
	ENDIF

c	IVIEW  =  NTFDEV(IUNIT,ISIDE)
c	CALL REFRESH_HF (IVIEW)

  999	CONTINUE
	RETURN
	END


		 * 
		 */
	}
	
	
	private double uploadTime = 0.0;
	public double getUploadTime(){return uploadTime;}
	public void setUploadTime(double d){
		uploadTime = Math.max(d, uploadTime);
		this.setDelay(d);
	}
	public boolean isUploading(double clock){
		return (uploadTime > clock);
	}
	
	
	/**
	 * Data relating to which other entity is currently being targeted
	 * 
	 * myTarget is the target entity this entity is currently aiming at, 
	 * null if none
	 * myWeapon is the weapon currently being used to aim at the target
	 */
	private Entity myTarget;
	public Entity getMyTarget(){return myTarget;}
	public void setMyTarget(Entity target){myTarget = target;}
	private EntityWeapon myWeapon;
	public EntityWeapon getMyWeapon(){return myWeapon;}
	public void setMyWeapon(EntityWeapon weapon){myWeapon = weapon;}
	
	private Entity commandTarget;
	public Entity getCommandTarget(){return commandTarget;}
	public void setCommandTarget(Entity target){commandTarget = target;}
	private int commandRounds;
	public int getCommandRounds(){return commandRounds;}
	public void setCommandRounds(int i){commandRounds = i;}
	private EntityWeapon commandWeapon; // cmdwpn ie use when command firing
	public void setCommandWeapon(EntityWeapon weapon){commandWeapon = weapon;}
	public EntityWeapon getCommandWeapon(){return commandWeapon;}
	public void clearComandFire(String message){
		setCommandRounds(0);
		setCommandTarget(null);
		if (message != null) informUser(message);
	}
	private EntityWeapon manualWeapon; //hf wpn ie use when manually firing
	public void setManualWeapon(EntityWeapon weapon){manualWeapon = weapon;}
	public EntityWeapon getManualWeapon(){return manualWeapon;}
	
	private double userMaxRange = 100.0; //TODO should default to platform max range
	public double getUserMaxRange(){return userMaxRange;}
	public void setUserMaxRange(double d){userMaxRange = d;}
	private double userMinRange = 0.0;
	public double getUserMinRange(){return userMinRange;}
	public void setUserMinRange(double d){userMinRange = d;}
	
	private int userTargetPriority = 0;
	public void setUserTargetPriority(int i){userTargetPriority = i;}
	public int getUserTargetPriority(){return userTargetPriority;}
	private double userPHThreshold = 0.0;
	public void setUserPhThreshold(double d){userPHThreshold = d;}
	public double getUserPHThreshold(){return userPHThreshold;}
	
	private int rateOfFire = Constants.ROF_AVERAGE;
	public int getRateOfFire(){return rateOfFire;}
	public void setRateOfFire(int i){
		if (i<0) return;
		if (i> Constants.ROF_HIGH) return;
		rateOfFire = i;
	}
	
	private Entity lastTarget = null;
	public Entity getLastTarget(){return lastTarget;}
	public void setLastTarget(Entity target){
		lastTarget = target;
	}
	private EntityWeapon lastWeapon = null;
	public EntityWeapon getLastWeapon(){return lastWeapon;}
	public void setLastWeapon(EntityWeapon weapon){lastWeapon = weapon;}
	
	public double getLastFireTime(){return 0.0;} //TODO
	public void setFiring(boolean b){}//TODO
	public double getTOF(){return 0.0;}//TODO clock time when TOF is done
	public double getFiringTimeMutiplier(EntityWeapon ew){return 1.0;
		/* TODO
	IF (KSQADWPN(IWSLOT,ITYPE) .LE. 0 ) THEN
	  TFACT = NSCORE(IUNIT,ISIDE)
	  TFACT = FLOAT(NFIR) / TFACT
	ELSE
	  TOT = NSCORE(IUNIT,ISIDE)
	  TOT = TOT * KSQADWPN(IWSLOT,ITYPE)
	  TOT = TOT / 100
	  TOT = INT(TOT + 0.999)
	  TFACT = FLOAT(NFIR) / TOT
	ENDIF

	IF (DEBUGDFIR .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	  WRITE (KLINE$,"('Time multiplication factor = ',F6.3)") TFACT
	  CALL TRACEOUT (0,0,0)
	ENDIF
		 * 
		 */
//TODO FTMP = ((TIMEFIR(IWPN)/60) * TFACT) apparently to account for number of elements?
	}
	
	public void informUser(String message){}//TODO
	public void shoot (double time, Entity target, EntityWeapon weapon){
		setFiring(true);
		setMyTarget(target);
		setMyWeapon(weapon);
	}
	public void clearTarget(){
		setMyTarget(null);
		setMyWeapon(null);
	}
	
	public int getHostility(Entity target){
		int level = this.getDetectionLevel(target);
		if ( level == Constants.OBSERVATION_LEVEL_UNSEEN)
			return Constants.HOSTILITY_UNSEEN;
		if ( level == Constants.OBSERVATION_LEVEL_DETECTED)
			return Constants.HOSTILITY_UNKNOWN;
		if ( level == Constants.OBSERVATION_LEVEL_RECOGNISED)
			return Constants.HOSTILITY_UNKNOWN;
		
		//TODO use force etc to work out hostility
		return Constants.HOSTILITY_ENEMY;
	}
	
	public boolean canShoot(Entity target){
		return true; // TODO based on hostility and roe can this target be shot
		// see checkroe
	}
	public boolean canAim(Entity target){
		return true; // TODO based on hostility and roe can this target be shot
	}
	
	
	public void exitPit(){} // TODO
	
	
	public void consumeFuel(double amount){ //TODO
		currentFuel -= amount;
		/*
		C------ If low fuel, warn player

		F_PERCENT = FUEL(IUNIT,ISIDE) / KAPACITY(ITYPE)

		IF( F_PERCENT .LE. 0.25 )  THEN
		    IF( ICNTWARN(IUNIT,ISIDE) .LT. 5 )  THEN
			ICNTWARN(IUNIT,ISIDE)  =  ICNTWARN(IUNIT,ISIDE) + 1
			CALL WARNFUEL ( IUNIT,ISIDE )
		    ENDIF
		ELSE
		  ! reset in case icon has been refueled
		  ICNTWARN(IUNIT,ISIDE) = 0
		ENDIF
		*/

	}
	
	private int bridgeState=0;
	private double bridgeTime = 0;
	public int getBridgeState(){return bridgeState;} // TODO
	public boolean recoverBridge(double clock){
		if (this.getPlatform().getBridgeLayer() == null) return true;
		if (bridgeState == Constants.BRIDGE_RECOVERED)return true;
		if (bridgeState == Constants.BRIDGE_RECOVERING){
			if (clock <= bridgeTime){
				bridgeState = Constants.BRIDGE_RECOVERED;
				return true;
			}
		} else { // deployed or deploying TODO
			bridgeState = Constants.BRIDGE_RECOVERING;
			double delay = clock + this.getPlatform().getBridgeLayer().getPackupTime();
			bridgeTime = delay;
		} //TODO finish this
		return false;
	}
	
	public boolean iamDigging(){return false;} // TODO
	public void updateMount(){
		/*
		C Need to update passengers too! for los array
			CALL UPDMOUNT ( IUNIT, ISIDE)
			 */
	}//TODO
	
	
	private double CBRTime = 0.0;
	public double getCBRTime(){return CBRTime;}
	public void setCBRTime(double d){CBRTime = Math.max(CBRTime, d);}
	private int CBRStatus = 0;
	public int getCBRStatus(){return CBRStatus;}
	public void setCBRStatus(int i){CBRStatus = i;} //TODO validate
	
	public double getBaseHeight(){return 0.0;} // TODO should be posture height or similar, but independent if terrain or altitude
	
	private boolean clearMines = false;
	public boolean isClearingMines(){return clearMines;}
	public void setClearMines(boolean b){clearMines = b;}
	
	private Coordinate moveTo = null;
	public Coordinate getMoveTo(){ return moveTo;}
	public void setMoveTo(Coordinate c){
		// TODO validate on map etc
		if (c!= null){
			moveTo = new Coordinate (c);
		} else {
			moveTo = null;
		}
	}
	
	private Coordinate circle = null;
	public Coordinate getCircle(){
		return circle;
	}
	public void setCircle(Coordinate c){
		// TODO validate
		circle = new Coordinate(c);
	}
	private double circleTime = 0;
	public double getCircleTime(){ return circleTime;}
	public void setCircleTime(double d){circleTime = d;}
	private double circleRadius = 0.0;
	public double getCircleRadius(){return circleRadius;}
	public void setRadius(double d){circleRadius = d;}
	private boolean circleClockwise = true;
	public boolean getCircleClockwise(){return circleClockwise;}
	public void setCircleClockwise(boolean b){circleClockwise = b;}
	@Override
	public void clearCircle(){
		circleRadius = 0.0;
		circleTime = 0.0;
		circle = null;
	}
	
	public boolean firedRecently(double time){
		// from getspeed
		// IF( (FLAST(IUNIT,ISIDE)+2.0) .GT. CLOCK )  THEN
		if ( (this.lastFiredTime + 120.0) > time) return true;
		return false;
	//FIXME should use a value from the parameter class
	}
	
	//TODO slow to fire
	public double getSlowToFire(){return 0.5;}
	
	
	public void removeFromBridge(MoverEntity entity){
		/*
		  IF (IMOVER .EQ. MOVFOOT ) THEN
		    BRIDGE_FOOT(IBRIDGE,ISIDE) = BRIDGE_FOOT(IBRIDGE,ISIDE) - 1

	D	    IF ((DEBUGMOVE .GT. 0) .AND. (ITRACK(IUNIT,ISIDE) .GT. 0) ) THEN
	D	      WRITE (KLINE$,"(8X,'Bridge now has ',I3,' foot remaining')") BRIDGE_FOOT(IBRIDGE,ISIDE)
	D	      CALL TRACEOUT (0,0,0)
	D	    ENDIF

		  ELSE

		    BRIDGE_VEHICLES(IBRIDGE,ISIDE) = BRIDGE_VEHICLES(IBRIDGE,ISIDE) - 1

	D	    IF ((DEBUGMOVE .GT. 0) .AND. (ITRACK(IUNIT,ISIDE) .GT. 0) ) THEN
	D	      WRITE (KLINE$,"(8X,'Bridge now has ',I3,' vehicles remaining')") BRIDGE_VEHICLES(IBRIDGE,ISIDE)
	D	      CALL TRACEOUT (0,0,0)
	D	    ENDIF

		  ENDIF

		  ON_BRIDGE(IUNIT,ISIDE) = 0

			 * 
			 */
		entity.enterBridge(null); // TODO could check return value
	}
	
	public void goUp(){ //TODO
		this.setDelay(10.0);
	}
	public void goDown(){
		this.setDelay(10.0);
		/*
		 * 
	SUBROUTINE  SET_GO_DOWN  ( INUNIT, ISIDE)

	include 	'global.f'
	include 	'globunits.f'
	include 	'globrpt.f'
	include 	'globmove.f'
c	include 	'globbut.f'
	include 	'globnode.f'
c	include		'globsetup.f'

	INTEGER*4	IUNIT, ISIDE, ITYPE, IBUILD, IFLOORS, IMODE

	IUNIT = INUNIT

	call validunit ( iunit, iside)
	if ( iunit .le. 0 ) return

	IF( MOUNTED(IUNIT,ISIDE) .GT. 0 )	    GOTO 100
	IF( NSCORE(IUNIT,ISIDE) .LT. 1 )	    GOTO 100
	ITYPE = KSYSTYP(IUNIT,ISIDE)
	IF ( MOVERS(ITYPE) .NE. MOVFOOT ) GOTO 100

	IMODE = IDEFPAR

	IF ( ON_ROOF(IUNIT,ISIDE) .GT.0) THEN
	  IF( CLOCK_TICKING )  THEN
	    IF ( TDELAY(IUNIT,ISIDE)  .GT. CLOCK ) THEN
	      WRITE (KLINE$,"('Icon is currently delayed, unable to change floor')")
	      CALL INFORM (IUNIT, ISIDE)
	      GOTO 100
	    ELSE
	      CALL SETDLAY ( IUNIT, ISIDE, 10.0) ! delay for 10 secs
	    ENDIF
	  ENDIF
	  IBUILD = ON_ROOF(IUNIT,ISIDE)
	  CALL TRRN_GET_BUILD_FLOORS ( IBUILD, IFLOORS)
	  ON_ROOF(IUNIT,ISIDE) = 0
	  ON_FLOOR(IUNIT,ISIDE) = IFLOORS - 1
	  IN_BUILDING(IUNIT,ISIDE) = IBUILD
	  AT_WALL(IUNIT,ISIDE) = 0
	  AT_WALL_ANGLE(IUNIT,ISIDE) = 0.0
	  IF( CLOCK_TICKING )  THEN
	    CALL SET_DEFILADE ( IUNIT, ISIDE, IMODE)
	  ENDIF
	  CALL REDRAW_ICON(IUNIT,ISIDE)

	ELSEIF ( IN_BUILDING(IUNIT,ISIDE) .GT. 0 ) THEN
	  IF ( ON_FLOOR(IUNIT,ISIDE) .GT. 0 ) THEN
	    IBUILD = IN_BUILDING(IUNIT,ISIDE)
	    CALL TRRN_GET_BUILD_FLOORS ( IBUILD, IFLOORS)
	    IF( CLOCK_TICKING )  THEN
	      IF ( TDELAY(IUNIT,ISIDE)  .GT. CLOCK ) THEN
	        WRITE (KLINE$,"('Icon is currently delayed, unable to change floor')")
	        CALL INFORM (IUNIT, ISIDE)
	        GOTO 100
	      ELSE
	        CALL SETDLAY ( IUNIT, ISIDE, 10.0) ! delay for 10 secs
	      ENDIF
	    ENDIF
	    ON_FLOOR(IUNIT,ISIDE) = ON_FLOOR(IUNIT,ISIDE) - 1
	    AT_WALL(IUNIT,ISIDE) = 0
	    AT_WALL_ANGLE(IUNIT,ISIDE) = 0.0
	    IF( CLOCK_TICKING )  THEN
	      CALL SET_DEFILADE ( IUNIT, ISIDE, IMODE)
	    ENDIF
	    CALL REDRAW_ICON(IUNIT,ISIDE)
	  ENDIF
	ENDIF

100	CONTINUE

999	CONTINUE
	RETURN
	END

	SUBROUTINE  SET_GO_UP  ( INUNIT, ISIDE)

	IMPLICIT NONE

	include 	'global.f'
	include 	'globunits.f'
	include 	'globrpt.f'
	include 	'globmove.f'
c	include 	'globbut.f'
	include 	'globnode.f'
c	include		'globsetup.f'

	INTEGER*4	IUNIT, ISIDE, ITYPE, IBUILD, IFLOORS, INUNIT, IMODE

	IUNIT = INUNIT

	call validunit ( iunit, iside)
	if ( iunit .le. 0 ) return

	IF( MOUNTED(IUNIT,ISIDE) .GT. 0 )	    GOTO 100
	IF( NSCORE(IUNIT,ISIDE) .LT. 1 )	    GOTO 100
	ITYPE = KSYSTYP(IUNIT,ISIDE)
	IF ( MOVERS(ITYPE) .NE. MOVFOOT ) GOTO 100

	IF ( IN_BUILDING(IUNIT,ISIDE) .LE. 0 ) THEN
	  GOTO 100
	ENDIF

	IMODE = IDEFPAR

	IBUILD = IN_BUILDING(IUNIT,ISIDE)
	CALL TRRN_GET_BUILD_FLOORS ( IBUILD, IFLOORS)

	IF ( ON_FLOOR(IUNIT,ISIDE) .LE. IFLOORS -2 ) THEN
	  IF( CLOCK_TICKING )  THEN
	    IF ( TDELAY(IUNIT,ISIDE)  .GT. CLOCK ) THEN
	      WRITE (KLINE$,"('Icon is currently delayed, unable to change floor')")
	      CALL INFORM (IUNIT, ISIDE)
	      GOTO 100
	    ELSE
	      CALL SETDLAY ( IUNIT, ISIDE, 10.0) ! delay for 10 secs
	    ENDIF
	  ENDIF
	  ON_FLOOR(IUNIT,ISIDE) = ON_FLOOR(IUNIT,ISIDE) + 1
	  AT_WALL(IUNIT,ISIDE) = 0
	  AT_WALL_ANGLE(IUNIT,ISIDE) = 0.0
	  CALL REDRAW_ICON(IUNIT,ISIDE)
	  IF( CLOCK_TICKING )  THEN
	    CALL SET_DEFILADE ( IUNIT, ISIDE, IMODE)
	  ENDIF
	ELSEIF ( ON_FLOOR(IUNIT,ISIDE) .EQ. (IFLOORS -1)) THEN
	  IF( CLOCK_TICKING )  THEN
	    IF ( TDELAY(IUNIT,ISIDE)  .GT. CLOCK ) THEN
	      WRITE (KLINE$,"('Icon is currently delayed, unable to change floor')")
	      CALL INFORM (IUNIT, ISIDE)
	      GOTO 100
	    ELSE
	      CALL SETDLAY ( IUNIT, ISIDE, 10.0) ! delay for 10 secs
	    ENDIF
	  ENDIF
	  ON_ROOF(IUNIT,ISIDE) = IBUILD
	  ON_FLOOR(IUNIT,ISIDE) = 0
	  IN_BUILDING(IUNIT,ISIDE) = 0
	  AT_WALL(IUNIT,ISIDE) = 0
	  AT_WALL_ANGLE(IUNIT,ISIDE) = 0.0
	  CALL REDRAW_ICON(IUNIT,ISIDE)
	  IF( CLOCK_TICKING )  THEN
	    CALL SET_DEFILADE ( IUNIT, ISIDE, IMODE)
	  ENDIF
	ENDIF

100	CONTINUE

999	CONTINUE
	RETURN
	END

		 */
	}
	
	public void resetFuelWarning(){
		// CALL RESETWARN ( IUNIT,ISIDE ) TODO
	}
	
	
	private boolean runMode = false;
	public boolean iAmRunning(){return runMode;}
	private boolean reverseMode = false;
	public boolean iAmReversing(){return reverseMode;}

	private boolean routeLocked = false; // TODO
	
	private int moveMode = 0;
	public int getMoveMode(){return moveMode;}
	public void setMoveMode(int i){
		switch (i){
		case Constants.MOVE_ADVANCE: {this.setAdvance(); break;}
		case Constants.MOVE_ASSAULT: {this.setAssault(); break;}
		case Constants.MOVE_RUN: {this.setRun(); break;}
		case Constants.MOVE_RUSH: {this.setRush(); break;}
		case Constants.MOVE_CRAWL: {this.setCrawl(); break;}
		case Constants.MOVE_REVERSE: {this.setReverse(); break;}
		case Constants.MOVE_CAUTIOUS: {this.setCautious(); break;}
		case Constants.MOVE_GROUP: {this.setGroupMove(); break;}
		default : Logger.err(0, "invalid move mode " + i); // TODO more info
		}
		moveMode = i;
	} // TODO validate
	
	private int flightMode = Constants.FLY_AGL;
	public int getFlightMode(){return flightMode;}
	
	public void setAssault(){
		if (routeLocked) return;
		if (this.getPlatform().getMoverType()!= Constants.MOVER_FOOT) return;
		crawlMode = false;
		runMode = true;
		reverseMode = false;
		this.moveMode = Constants.MOVE_ASSAULT;
		this.setCommandSpeed(this.getPlatform().getSpeedRunning());
	}
	
	public void setRush(){
		if (routeLocked) return;
		if (this.getPlatform().getMoverType()!= Constants.MOVER_FOOT) return;
		crawlMode = false;
		runMode = true;
		reverseMode = false;
		this.moveMode = Constants.MOVE_RUSH;
		this.setCommandSpeed(this.getPlatform().getSpeedRunning());
	}
	public void setCautious(){
		if (routeLocked) return;
		if (this.getPlatform().getMoverType()!= Constants.MOVER_FOOT) return;
		crawlMode = true;
		runMode = false;
		reverseMode = false;
		this.moveMode = Constants.MOVE_CAUTIOUS;
		this.setCommandSpeed(this.getPlatform().getSpeedCrawling());
	}
	public void setAdvance(){
		if (routeLocked) return;
		if (this.getPlatform().getMoverType()!= Constants.MOVER_FOOT) return;
		crawlMode = false;
		runMode = false;
		reverseMode = false;
		this.moveMode = Constants.MOVE_ADVANCE;
		this.setCommandSpeed(this.getPlatform().getSpeedCountry());
	}
	public void setReverse(){
		if (routeLocked) return;
		//NOTE foot movers can't reverse
		if (this.getPlatform().getMoverType()== Constants.MOVER_FOOT) return;
		// not flyers? TODO
		crawlMode = false;
		runMode = false;
		if (reverseMode){
			this.moveMode = Constants.MOVE_NORMAL;
			this.setCommandSpeed(this.getPlatform().getSpeedCountry());
			reverseMode = false;
		} else {
			this.moveMode = Constants.MOVE_REVERSE;
			this.setCommandSpeed(this.getPlatform().getSpeedReverse());
			reverseMode = true;
		}
	}
	public void setCrawl(){
		if (routeLocked) return;
		//NOTE all ground movers can crawl
		//TODO flyers?
		crawlMode = true;
		runMode = false;
		reverseMode = false;
		this.moveMode = Constants.MOVE_CRAWL;
		this.setCommandSpeed(this.getPlatform().getSpeedCrawling());
	}
	public void setSprint(){
		if (routeLocked) return;
		//TODO Flyers?
		crawlMode = false;
		runMode = false;
		reverseMode = false;
		this.moveMode = Constants.MOVE_NORMAL;
		this.setCommandSpeed(this.getPlatform().getSpeedCountry());
	}
	public void setRun(){
		if (routeLocked) return;
		if (this.getPlatform().getMoverType()!= Constants.MOVER_FOOT) return;
		crawlMode = false;
		runMode = true;
		reverseMode = false;
		this.moveMode = Constants.MOVE_RUN;
		this.setCommandSpeed(this.getPlatform().getSpeedRunning());
	}
	public void setGroupMove(){
		if (routeLocked) return;
		//TODO Flyers?
		Force force = this.getForce();
		if (force == null) return;
		crawlMode = false;
		runMode = false;
		reverseMode = false;
		this.moveMode = Constants.MOVE_GROUP;
		this.setCommandSpeed(force.getSpeed());
	}
	
	private int viewMode = Constants.VIEW_NORMAL;
	public int getViewMode(){return viewMode;}
	public void setViewMode(int i){viewMode = i;}
	
	private Route myRoute;
	public Route getRoute(){return myRoute;}
	public void setRoute(Route route){
		myRoute = route;
		//TODO should we copy the route so it can be locally changed
	}
	public void addNode(Node node){
		if (myRoute == null) myRoute = new Route();
		myRoute.add(node);
	}
	
	private boolean stopped = false;
	public boolean isStopped(){return stopped;}
	public void stop(String s){
		this.stop();
	} // stop entity and send string to GUI
	
	public void stop(){
		stopped = true;
	}

	public void go(){
		stopped = false;
		holding = false;
		holdTime = 0;
	}
	private boolean holding = false;
	public boolean isHolding(){return holding;}
	public void setHold(double time){
		holdTime = time; //TODO could validate
		holding = true;
	}
	private double holdTime = 0.0;
	public double getHoldTime(){return holdTime;}
	
	private MoverEntity formationLeader = null;
	public MoverEntity getFormationLeader(){return formationLeader;}
	//TODO when setting leader, should also set altitude and flight mode to match
	private MoverEntity convoyLeader = null;
	public MoverEntity getConvoyLeader(){return convoyLeader;}
	
	public void alert (String message){} // TODO should somehow give a message to user
	// we need an entity listener class
	public void warnFuel(){
		/*
		 * 
		 */
	}
	
	private Obstacle constructingObstacle = null;
	public Obstacle getConstructingObstacle(){return constructingObstacle;}
	public void setConstructingObstacle(Obstacle o){ constructingObstacle = o;}
	
	private double curentSize = 10; //TODO size, needs to account for posture and platform type
	public double getApparentSize(double range){return 10.0;} // see getsize.f
	public double getRealSize(){return 10.0;}
	public int getContrastClass(){return 1;} //TODO should use posture and platform

}
