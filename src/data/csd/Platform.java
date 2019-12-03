package data.csd;

import java.util.HashMap;
import java.util.Vector;

import data.managers.Identifiable;
import data.managers.SensorList;
import interpreterOLD.FileLoader;
import interpreterOLD.InterpreterInterface;
import sim.Constants;

/**
 * Define class to represent system data.
 * See globsysc.f
 *
 */
public class Platform implements Identifiable {
	public static void main(String[] args){
		Platform me = new Platform();
		System.out.println(me.toText());
	}
	
	public Platform (){}
	public Platform (String name){
		this.name = name;
	}

	private String name = "test_platform";

	@Override
	public String getName(){
		return this.name.substring(0);
	}
	
	@Override
	public void setName(String name) {
		// TODO Auto-generated method stub
		
	}
	@Override
	public int getNumber() {
		// TODO Auto-generated method stub
		return 0;
	}
	@Override
	public void setNumber(int number) {
		// TODO Auto-generated method stub
		
	}
	
	private String movementModel = "";
	public String getMovementModel(){
		return movementModel.substring(0);
	}
	public void setMovementModel(String model){
		movementModel = model.substring(0);
	}
	private String scanModel = "";
	public String getScanModel(){
		return scanModel.substring(0);
	}
	public void setScanModel(String model){
		scanModel = model.substring(0);
	}
	private String detectModel = "";
	public String getDetectModel(){
		return detectModel.substring(0);
	}
	public void setDetectModel(String model){
		detectModel = model.substring(0);
	}
	private String detectObstacleModel = "";
	public String getDetectObstacleModel(){
		return detectObstacleModel.substring(0);
	}
	public void setDetectObstacleModel(String model){
		detectObstacleModel = model.substring(0);
	}
	private String shootModel = "";
	public String getShootModel(){
		return shootModel.substring(0);
	}
	public void setShootModel(String model){
		shootModel = model.substring(0);
	}
	private String suppressionModel = "";
	public String getSuppressionModel(){
		return suppressionModel.substring(0);
	}
	public void setSuppressionModel(String model){
		shootModel = model.substring(0);
	}
	private String resupplyModel = "";
	public String getResupplyModel(){
		return resupplyModel.substring(0);
	}
	public void setResupplyModel(String model){
		resupplyModel = model.substring(0);
	}
	private String casualtyModel = "";
	public String getCasualtyModel(){
		return casualtyModel.substring(0);
	}
	public void setCasualtyModel(String model){
		casualtyModel = model.substring(0);
	}
	//TODO add other models

	/*
	 * Basic characteristics
	 */
	
	// length in m for purpose of detection
	private double length = 0.5; // globsysc.csyslen
	public double getLength(){return length;}
	public void setLength(double len) {length = len;}
	// width in m
	private double width = 0.5; // globsysc.csyswid 
	public double getWidth(){return width;}
	public void setWidth(double wid){width = wid;}
	// height in m
	private double height = 1.7; // globsysc.csyshgt
	public double getHeight(){return height;}
	public void setHeight( double hei) {height = hei;}
	// number of crew
	private int crewSize = 1; // globsysc.kcrewc 
	public void setCrewSize(int i){crewSize = i;}
	public int getCrewSize(){return crewSize;}
	// element spacing for homogenous aggregation
	private double elementSpacing = 5.0; // globsysc.keledistc 
	public void setElementSpacing(double d){elementSpacing = d;}
	public double getElementSpacing(){return elementSpacing;}
	
	/*
	 * Data relating to weapons
	 */
	
	// can the platform fire direct or indirect
	private boolean canFireDirect = true;  // globsysc.cfirers
	private boolean canFireIndirect = true;  // globsysc.cfirers
	// primary weapon range in km
	private double primaryRange = 1.0; //globsysc.cprimrng 
	public void setPrimaryRange (double range){primaryRange = range;}
	public double getPrimaryRange(){return primaryRange;}
	
	private Vector<PlatformWeapon> weaponList = 
			new Vector<PlatformWeapon>(); // globsysc.numrelwpns
	public void addWeapon(PlatformWeapon weapon){
		weaponList.add(weapon);
	}
	public Vector<PlatformWeapon> getWeaponList(){
		return weaponList;
	}
	
	private String defaultCommandWeapon = null; // globsysc.default_cmd_wpnc
	private String defaultAreaWeapon = null; // globsysc.default_area_wpnc
	private HashMap<Platform, Double> rangeBreak = 
			new HashMap<Platform, Double>(); //globsysc.crngbrk
	private HashMap<Platform,PlatformWeapon> preferredWeaponClose = 
			new HashMap<Platform,PlatformWeapon>(); // globsysc.kwpnrelc TODO
	private HashMap<Platform,PlatformWeapon> preferredWeaponLong = 
			new HashMap<Platform,PlatformWeapon>(); // globsysc.kwpnrelc TODO

	/**
	 * data relating to movement
	 */
	private int moverType = Constants.MOVER_FOOT; // globsysc.cmovers
	public int getMoverType(){return moverType;}
	public void setMoverType(int i){moverType = i;} // TODO validate
	private boolean canSwim = false;  // globsysc.cswimmrs
	public boolean canSwim(){return canSwim;}
	public void canSwim(boolean b){canSwim = b;}
	private boolean canFord = false;  // globsysc.cswimmrs
	// fording depth in m
	private double fordingDepth = 0.0; // globsysc.cfording_depth
	// max velocity in kph
	private double maxVelocity = 100.0; // globsysc.kmaxvelc 
	public double getMaxVelocity(){return maxVelocity;}
	public void setMaxVelocity(double d){maxVelocity = d;}
	public double getSpeedRoad(){return maxVelocity;}
	public void setSpeedRoad(double speed){maxVelocity = speed;}
	
	// max speed while reversing in kph
	private double speedReverse = 20.0; // globsysc.speed_reversec 
	public double getSpeedReverse(){return speedReverse;}
	public void setSpeedReverse(double speed){speedReverse = speed;}
	//max speed off road in kph
	private double speedCountry = 20.0; // globsysc.speed_countryc 
	public double getSpeedCountry(){return speedCountry;}
	public void setSpeedCountry(double speed){speedCountry = speed;}
	// max speed while running in kph
	private double speedRunning = 20.0; // globsysc.speed_runningc
	public double getSpeedRunning(){return speedRunning;}
	public void setSpeedRunning(double speed){speedRunning = speed;}
	// max speed when crawling in kph
	private double speedCrawling = 5.0; // globsysc.crawlspeedc
	public double getSpeedCrawling(){return speedCrawling;}
	public void setSpeedCrawling(double speed){speedCrawling = speed;}

	// data used for speed on slope calculations - see veloc.f
	private double slopeFactor = -0.02; // see veloc.f wheels = -0.02, track/ foot = -0.01667
	public double getSlopeFactor(){return slopeFactor;}
	public void setSlopeFactor(double d){slopeFactor = d;}
	private double maxSlope = 0.49; // see veloc.f max slop = wheel 49%, track 59%
	public double getMaxSlope(){return maxSlope;}
	public void setMaxSlope(double d){maxSlope = d;}
	
	/*
	 * Data relating to sensors
	 */
	//TODO should this be smarter than a vector?
	private SensorList sensorList = new SensorList(); // globsysc.csysensors
	public void addSensor(Sensor sensor){
		if (sensor == null)return;
		sensorList.add(sensor);
	}
	public SensorList getSensorList(){
		return sensorList;
	}
	// visibility in km
	private double maxVisibility = 5.0; // globsysc.maxvisc maximum
	public void setMaxVisibility(double d){maxVisibility = d;}
	public double getMaxVisibility(){return maxVisibility;}
	// Field of view minimum degree
	private double fovMin = 15.0;
	// field of view max degree
	private double fovMax = 360.0;

	/*
	 * Data relating to detection probability
	 */
	private double detectionSize = 1.0; // globsysc.detectsizec
	public void setDetectionSize(double d){detectionSize = d;}
	public double getDetectionSize(){return detectionSize;}
	private double detectionSizeCrawling = 0.5; // globsysc.crawlsizec
	private double detectionSizePartialDefilade = 0.5; // globsysc.partialsizec
	private double detectionSizeFullDefilade = 0.2; // globsysc.defiladesizec
	private double detectionSizePit = 0.1; // globsysc.pitsizec 
	private double recognitionThreshold = 1.0; // globsysc.system_recogc
	private double identificationThreshold = 2.0; // globsysc.system_identc
	private double runFactorPD = 0.1; // TODO need get/ set
	public double getRunFactorPD(){return runFactorPD;} 
	private double moveFactorPD = 0.5;
	public double getMoveFactorPD(){return moveFactorPD;}

	/*
	 * Data relating to constituent classes
	 */
	
	// is this platform a flyer? pointer to type data or string?
	private Flyer flyerType = null; // globsysc.cflyers 
	public Flyer getFlyerType(){return flyerType;}
	public void setFlyerType (Flyer f){flyerType = f;}
	// is the platform a fuel tanker
	private Tanker tankerType = null; // globsysc.clogstcs 
	// is this platform an ammo supplier
	private Resupply ammoSupplier = null; // globsysc.clogstcs 
	// type of laser designator
	private Designator designator = null; // globsysc.cdesgntr 
	private BridgeLayer bridgeType = null; // globsysc.bridge_layerc
	public BridgeLayer getBridgeLayer(){return bridgeType;}
	private EngineerType engineerType = null; // globsysc.cengneer
	// what type of air defence radar
	private ADRadar adRadarType = null; // globsysc.cradars 
	private CounterBatteryRadar cbr = null; // TODO used if adradar > 20
	public void setCBR(CounterBatteryRadar radar){cbr = radar;}
	public CounterBatteryRadar getCBRType(){return cbr;}
	
	/*
	 * Data relating to carrying capacity
	 */
	private int carrierType = Constants.NO_PASSENGERS;// globsysc.ckanhost
	private double weight = 20; // globsysc.ksyswgtc weight in kg when fully loaded
	private double volume = 20; // globsysc.ksysvolc volume (m^2) when fully loaded
	private double carryWeight = 20; // globsysc.kcrywgtc extra carrying capacity in kg
	private double carryVolume = 20; // extra carrying capacity in m^2
	
	/*
	 * Data relating to fuel and ammunition supply
	 */
	// fuel capacity in litres
	private double fuelCapacity = 100.0; // globsysc.ckapacity 
	public double getFuelCapapcity(){return fuelCapacity;}
	public void setFuelCapacity(double capapcity){fuelCapacity = capapcity;}
	// fuel consumption per hr while moving
	private double fuelConsumptionMoving = 1.0; // globsysc.ckonsumptn 
	public double getFuelConsumptionMoving(){return fuelConsumptionMoving;}
	// fuel consumption per hr while stationary
	private double fuelConsumptionStationary = 1.0; // globsysc.ckonsumptn
	public double getFuelConsumptionStationary(){return fuelConsumptionStationary;}
	// fuel type
	private int fuelType = Constants.NO_FUEL; // globsysc.ckfueltype 
	// time after entity stops moving to turn off engine
	private double engineStopTime = 300.0; // globsysc.stop_engine  
	public double getEngineStopTime(){return engineStopTime;}
	public void setEngineStopTime(double d){engineStopTime = d;}
	
	/*
	 * Data relating to suppression performance
	 */
	
	// TODO add to output
	//degradation factor for PH when suppressed
	private double suppressionFactorPH = 0.5; // globsysc.supphfctc 
	public void setSuppressionFactorPH(double d){suppressionFactorPH = d;}
	public double getSuppressionFactorPH(){return suppressionFactorPH;}
	//degradation factor for PD when suppressed
	private double suppressionFactorPD = 0.5; // globsysc.suppdfctc  
	public void setSuppressionFactorPD(double d){suppressionFactorPD = d;}
	public double getSuppressionFactorPD(){return suppressionFactorPD;}
	// degradation factor for movement when suppressed
	private double suppressionFactorMove = 0.5; // globsysc.supmvfctc 
	public void setSuppressionFactorMove(double d){suppressionFactorMove = d;}
	public double getSuppressionFactorMove(){return suppressionFactorMove;}
	
	/*
	 * Data relating to active defences
	 */
	
	private ActiveDefenceType pointDefence = null; // globsys.sys_point_defence_type
	private int pointDefenceRounds = 0;// globsysc.sys_point_defence_roundsc
	private int pointDefenceStored = 0; // globsysc.sys_point_defence_storedc
	private ArmourType armour = null; // globsysc.sys_armourc
	private int armourPanels = 0; // globsysc.sys_armour_panelsc
	
	/*
	 * Data relating to miscellaneous behaviour
	 */
	
	private HashMap<String, Integer> targetPriority = 
			new HashMap<String, Integer>(); // globsysc.ckfirpri
	public void setTargetPriority(String targetName, int priority){
		targetPriority.put(targetName, (Integer) priority); 
	}
	public int getTargetPriority(String targetName){
		Integer i = targetPriority.get(targetName);
		if ( i == null) return 0;
		return i.intValue();
	}
	private HashMap<String, WeaponChoice> weaponChoice = 
			new HashMap<String, WeaponChoice>();
	public void addWeaponChoice (WeaponChoice choice){
		String name = choice.getPlatform().getName();
		weaponChoice.put(name, choice);
	}
	public PlatformWeapon getWeaponChoice (String PlatformName, double range){
		WeaponChoice choice = weaponChoice.get(PlatformName);
		if (choice == null) return null;
		if (range > choice.getRange()){
			return choice.getLongWeapon();
		} else {
			return choice.getCloseWeapon();
		}
	}
	
	// able to detect chemical weapons
	private boolean canDetectChemicals = false; // globsysc.csurveil 

	private boolean canPop = false; // globsysc.ckanpop
	private HashMap<Platform, Integer> detectionPriority = 
			new HashMap<Platform, Integer>(); // globsysc.cdetect_priority
	public int getDetectionPriority(Platform target){
		Integer priority = this.detectionPriority.get(target);
		if (priority == null) {
			return 0;
		}
		return priority.intValue();
	}
	public void setDetectionPriority(Platform target, int priority){
		if (priority < 0) return;
		if (priority > 10) return;
		detectionPriority.put(target, new Integer(priority));
	}
	private boolean medic = false; // globsysc.kmedicc
	
	private double speedCraters = 1.0; // speed factor when crossing craters
	public double getSpeedCraters(){return speedCraters;} //TODO
	
	/*
	 * Symbology
        INTEGER*4  CSYMRECOG (NUMCSDTYPES)
  *		CSYMRECOG,		! Symbol to use when this system is
					! detected at level of "recognition".
	 */

	/*

     *		MAXVISC_DND,
                                        ! look for detection purposes. (km)

C ----- COMMON  / GLOBSYSC3 /

	INTEGER*4	NUM_ACTIVE_TYPEC
	PARAMETER ( NUM_ACTIVE_TYPEC = 5 )


	REAL	CSYSENSHI  (8,NUMCSDTYPES)

	BYTE	CKAN_DETECT_SENSOR(NUMCSDTYPES, NUM_ACTIVE_TYPEC)
	BYTE	KMINDISPC (NUMCSDTYPES)
	BYTE	MINE_DISPENSERC(NUMCSDTYPES,3)
	REAL	MINE_DELAYC (NUMCSDTYPES,3)
	INTEGER*4	MINEFIELD_LOADC(NUMCSDTYPES,3)
	BYTE	KSMKDISPC (NUMCSDTYPES)
        BYTE    KREPAIRC  (NUMCSDTYPES)
        BYTE    KCREWMENC (NUMCSDTYPES)

	COMMON  / GLOBSYSC3 /

     *		CSYSENSHI,
     *		CKAN_DETECT_SENSOR,

     *  	KMINDISPC,		! Mine Dispenser
     *		MINE_DISPENSERC,
     *		MINE_DELAYC,
     *		MINEFIELD_LOADC,

*/
	/*

     *  	KSMKDISPC,		! Smoke Dispenser
						    
*/
	/*
     *          KREPAIRC,               ! Repair Type
                                        !     1 - Can perform field repairs
                                        !     2 - Can haul/tow wheeled vehicles
                                        !     3 - Can haul/tow any vehicle

     *          KCREWMENC,               ! Crew replacement for vehicles with
                                        ! a crew kill.
                                        !     0 - Cannot crew a vehicle
                                        !     1 - Can crew a vehicle

*/
	/*

	INTEGER*4	OBST_CLEAR_TIMEC(NCSDWTHR, NUMCSDTYPES, MAX_OBST_TYPEC)
	INTEGER*4	OBST_CROSS_TIMEC(NCSDWTHR, NUMCSDTYPES, MAX_OBST_TYPEC)
	REAL*4		OBST_PMOBC(NCSDWTHR, NUMCSDTYPES, MAX_OBST_TYPEC)

	 */
	//CSYSHEIGHT(NUMCSDTYPES,10), ! system height for different postures
	// CFOVPRIMARY(NUMCSDTYPES),
//	    *			CSLOPE_DATA(NUMCSDTYPES,3),
//	    *			CTARGET_HEIGHT(NUMCSDTYPES,0:10),
//	    *			CPOSTURE_HEIGHT(NUMCSDTYPES,0:10)

	/*
	 * 
C-------- FILENAME:  GLBMINES.FOR  ----------- Prepared by:   T. W. Peterson



C	Number of anti armor mines
	INTEGER*4	NUMAAM
	PARAMETER ( NUMAAM = 10 )
C	First index of anti pers mines
	INTEGER*4	NUMAPF
	PARAMETER ( NUMAPF = 11 )
C	Last index of anti pers mines
	INTEGER*4	NUMAPL
	PARAMETER ( NUMAPL = 16 )
C	Number of basic clearing types
	INTEGER*4	NUMCLRTYP
	PARAMETER ( NUMCLRTYP = 5 )

	INTEGER*4	NUM_MINE_KILLS
	PARAMETER ( NUM_MINE_KILLS = 5)


	INTEGER*2	MINE_DESTRUCT(NUMMINTY)	! can this mine type be self destructed
	REAL		MINE_DESTRUCT_PROB(NUMMINTY)

	REAL*4		DETBUT, DETUNB, DETINF, VISDIS, APWLEN, APANGL1, APANGL2
	INTEGER*4	KMWIRES
	REAL*4		PKMRANG, PKSOPEN, PKSWOOD, PKSTOWN


        COMMON  /MINEDAT1/

     *		MINE_DESTRUCT,
     *		MINE_DESTRUCT_PROB,


     *		DETBUT(NUMMINTY),                       ! The probability that
                                                        !  a vehicular unit can
                                                        !  detect this mine
                                                        !  given that the mine
                                                        !  is in the units
                                                        !  vision path and the
                                                        !  unit is buttoned.

     *  	DETUNB(NUMMINTY),                       ! The probability that
                                                        !  a vehicular unit can
                                                        !  detect this mine
                                                        !  given that the mine
                                                        !  is in the units
                                                        !  vision path and the
                                                        !  unit is un-buttoned.

     *  	DETINF(NUMMINTY),                       ! The probability that
                                                        !  an infantry unit can
                                                        !  detect this mine
                                                        !  given that the mine
                                                        !  is in the units
                                                        !  vision path.

     *          VISDIS(3),                              ! Distance units look
                                                        ! to the side for mine
                                                        ! detection purposes.
                                                        !  1 = unbuttoned
                                                        !  2 = buttoned
                                                        !  3 = dismounted
                                                        !      infantry

     *  	KMWIRES(NUMAPF:NUMAPL),                 ! The number of wires
                                                        !  in each anti
                                                        !  personnel mine.

     *  	APWLEN(NUMAPF:NUMAPL),                  ! The length of each
                                                        !  wire (km) for anti-
                                                        !  personnel mines.

     *  	APANGL1(NUMAPF:NUMAPL),                 ! First directional
                                                        !  angle for anti
                                                        !  personnel mines.

     *  	APANGL2(NUMAPF:NUMAPL),                 ! Second directional
                                                        !  angle for anti
                                                        !  personnel mines.


     *  	PKMRANG(5,NUMAPF:NUMAPL),               ! Ranges (km) for
                                                        !  anti personnel
                                                        !  mine kill probs.

     *  	PKSOPEN(5,NUMAPF:NUMAPL),               ! P(k) standing in
                                                        !  the open vs range
                                                        !  for anti pers. mines

     *  	PKSWOOD(5,NUMAPF:NUMAPL),               ! P(k) standing in
                                                        !  woods vs range
                                                        !  for anti pers. mines

     *  	PKSTOWN(5,NUMAPF:NUMAPL)		! P(k) standing in
                                                        !  town vs range
                                                        !  for anti pers. mines

*/
	private double speedMineClear = 3.0; // globmines/SYSMCLR_SPEED(NUMTYPES
	public double getSpeedMineClear(){return speedMineClear;}
	public void setSpeedMineClear(double speed){speedMineClear = speed;}
/*
	REAL
     *			SYSMCLR_WIDTH(NUMTYPES),
     *			SYSMCLR_LENGTH(NUMTYPES),
     *			SYSMCLR_SPEED(NUMTYPES),
     *			SYSMCLR_RELIABILITY(NUMAAM,NUMTYPES),
     *			SYSMCLR_SURVIVABILITY(NUMAAM,NUMTYPES),
     *			SYSMCLR_DETECTION(NUMAAM,NUMTYPES,2), ! detection probability unbuttoned, suppressed
     *			SYSMCLR_DISTANCE(NUMAAM,NUMTYPES,2) ! distance at which this type of mine can be detected

	INTEGER*4	SYSMCLR_MODE(NUMTYPES),
     *			SYSMCLR_DEVICES(NUMTYPES)

	REAL*4		SYSTRACK, SYSBELLY, SYSSHADW, PATRACK, PABELLY, PASHADW
	REAL*4		PKTRACK, PKBELLY, PKSHADW, CLRVEL, CLRWID, CLRREL, CLRSUR


	COMMON  /MINEDAT2/

     *		SYSTRACK(NUMTYPES),    	! Track width in km
                                                        !  for each system.

     *  	SYSBELLY(NUMTYPES),    	! Belly width in km
                                                        !  for each system.

     *  	SYSSHADW(NUMTYPES),     	! Magnetic shadow width
                                                        !  in km for each
                                                        !  system.

     *  	PATRACK(NUMAAM,NUMTYPES),	! Probability of mine
                                                        !  activation given
                                                        !  that it is under the
                                                        !  system's track.

     *  	PABELLY(NUMAAM,NUMTYPES),	! Probability of mine
                                                        !  activation given
                                                        !  that it is under the
                                                        !  system's belly.


     *  	PASHADW(NUMAAM,NUMTYPES),	! Probability of mine
                                                        !  activation given
                                                        !  that it in the
                                                        !  system's magnetic
                                                        !  shadow.

     *  	PKTRACK(NUM_MINE_KILLS,NUMAAM,NUMTYPES),	! Probability of system
                                                        !  kill given mine
                                                        !  activation under the
                                                        !  system's track.

     *  	PKBELLY(NUM_MINE_KILLS,NUMAAM,NUMTYPES),	! Probability of system
                                                        !  kill given mine
                                                        !  activation under the
                                                        !  system's belly.

     *  	PKSHADW(NUM_MINE_KILLS,NUMAAM,NUMTYPES),	! Probability of system
                                                        !  kill given mine 
                                                        !  activation under the
                                                        !  system's magnetic
                                                        !  shadow.
     
     *          CLRVEL(NUMCLRTYP),                      ! Velocity in km/hr
                                                        !  of each clearing 
                                                        !  type.
                                                        !   1 = plow        
                                                        !   2 = roller
                                                        !   3 = line charge
                                                        !   4 = bull by pushing
                                                        !   5 = dismounted inf

     *          CLRWID(NUMCLRTYP),                      ! Width in km of the
                                                        !  path the clearing
                                                        !  type clears.
                                                        
     *          CLRREL(NUMCLRTYP,NUMMINTY),             ! Reliability of clear-
                                                        !  ing device in 
                                                        !  neutralizing mines
                                                        !  by mine type given
                                                        !  a mine encounter.

     *          CLRSUR(NUMCLRTYP,NUMMINTY),              ! Survivability of
                                                        !  clearing device by
                                                        !  mine type given a
                                                        !  mine encounter.


     *			SYSMCLR_WIDTH,
     *			SYSMCLR_LENGTH,
     *			SYSMCLR_SPEED,
     *			SYSMCLR_RELIABILITY,
     *			SYSMCLR_SURVIVABILITY,
     *			SYSMCLR_DETECTION,
     *			SYSMCLR_DISTANCE,
     *			SYSMCLR_MODE,
     *			SYSMCLR_DEVICES


	*/

	/*

day/ night / dusk versions of speed
	INTEGER*4	KMAXVELC_DND( NUMCSDTYPES,3)
	REAL*4		SPEED_COUNTRYC_DND(NUMCSDTYPES,3)
     *			MAXVISC_DND(NUMCSDTYPES, 3),
	
	*/
	
	//from globartc TODO
	private int vulnerabilityExposed = 0;
	public int getVulnerabilityExposed(){return vulnerabilityExposed;}
	public void setVulnerabilityExposed(int i) {vulnerabilityExposed = i;}
	private int vulnerabilityDefilade = 0;
	public int getVulnerabilityDefilade(){return vulnerabilityDefilade;}
	public void setVulnerabilityDefilade(int i) {vulnerabilityDefilade = i;}

	public String toText (){
		String newline = "\n";
		String s = "";
		s = s + "platform " + this.name + newline;
		s = s + "length " + this.length + newline;
		s = s + "width " + this.width + newline;
		s = s + "height " + this.height + newline;
		s = s + "crew " + this.crewSize + newline;
		s = s + "spacing " + this.elementSpacing + newline;
		s = s + "direct_fire " + this.canFireDirect + newline;
		s = s + "indirect_fire " + this.canFireIndirect + newline;
		s = s + "range " + this.primaryRange + newline;
		if ( defaultCommandWeapon != null){
			s = s + "command_weapon " + defaultCommandWeapon + newline;			
		}
		if ( defaultAreaWeapon != null){
			s = s + "area_weapon " + defaultAreaWeapon + newline;			
		}
/*
 * weaponlist
		private HashMap<Platform, Double> rangeBreak = 
				new HashMap<Platform, Double>(); //globsysc.crngbrk
		private HashMap<Platform,String> preferredWeaponClose = 
				new HashMap<Platform,String>(); // globsysc.kwpnrelc TODO
		private HashMap<Platform,String> preferredWeaponLong = 
				new HashMap<Platform,String>(); // globsysc.kwpnrelc TODO
*/
		
		s = s + "mover " + Constants.MOVER_TYPE[this.moverType] + newline;
		s = s + "swim " + this.canSwim + newline;
		s = s + "ford " + this.canFord + newline;
		s = s + "fording_depth " + this.fordingDepth + newline;
		s = s + "max_speed " + this.maxVelocity + newline;
		s = s + "speed_reverse " + this.speedReverse + newline;
		s = s + "speed_country " + this.speedCountry + newline;
		s = s + "speed_run " + this.speedRunning + newline;
		s = s + "speed_crawl " + this.speedCrawling + newline;
		s = s + "slope_factor " + this.slopeFactor + newline;
		s = s + "max_slope " + this.maxSlope + newline;
		
		// sensorlist
		s = s + "visibility " + this.maxVisibility + newline;
		s = s + "fov_min " + this.fovMin + newline;
		s = s + "fov_max " + this.fovMax + newline;

		s = s + "size_exposed " + this.detectionSize + newline;
		s = s + "size_partial " + this.detectionSizePartialDefilade + newline;
		s = s + "size_defilade " + this.detectionSizeFullDefilade + newline;
		s = s + "size_crawl " + this.detectionSizeCrawling + newline;
		s = s + "size_pit " + this.detectionSizePit + newline;
		s = s + "recognise " + this.recognitionThreshold + newline;
		s = s + "identify " + this.identificationThreshold + newline;

		/*
		private Flyer flyerType = null; // globsysc.cflyers 
		private Tanker tankerType = null; // globsysc.clogstcs 
		private Resupply ammoSupplier = null; // globsysc.clogstcs 
		private Designator designator = null; // globsysc.cdesgntr 
		private BridgeLayer bridgeType = null; // globsysc.bridge_layerc
		private EngineerType engineerType = null; // globsysc.cengneer
		private ADRadar adRadarType = null; // globsysc.cradars 
		private CounterBatteryRadar cbr = null; // TODO used if adradar > 20
		*/

		s = s + "passenger_mode " + Constants.CARRIER_TYPE[this.carrierType] + newline;
		s = s + "weight " + this.weight + newline;
		s = s + "volume " + this.volume + newline;
		s = s + "carry_weight " + this.carryWeight + newline;
		s = s + "carry_volume " + this.carryVolume + newline;

		s = s + "fuel_capacity " + this.fuelCapacity + newline;
		s = s + "fuel_consumption_moving " + this.fuelConsumptionMoving + newline;
		s = s + "fuel_consumption_stationary " + this.fuelConsumptionStationary + newline;
		s = s + "fuel " + Constants.FUEL_TYPE[this.fuelType] + newline;
		s = s + "engine_stop_time " + this.engineStopTime + newline;

		return s;
	}
	
	/*
	 * public accessor methods
	 */
	
	/*
	 * from globunits
	INTEGER*4	ISPDT (NUMTYPES)	! Max road speed (Km/Hr)
	BYTE 	NPEOP (NUMTYPES)	! Number of crew
	REAL		SPEED_REVERSE(NUMTYPES)
	REAL		SPEED_COUNTRY(NUMTYPES)
	REAL		SPEED_RUNNING(NUMTYPES)

	INTEGER*4	KMAXVEL_DND(NUMTYPES, 3)
	REAL*4		SPEED_COUNTRY_DND(NUMTYPES, 3)

        COMMON / NPEOP /
     *		NPEOP,
     *		ISPDT,
     *		SPEED_REVERSE,
     *		SPEED_COUNTRY,
     *		SPEED_RUNNING,
     *		KMAXVEL_DND,
     *		SPEED_COUNTRY_DND

	INTEGER*4	KINSYM (NUMTYPES)	! Graphic symbol (static)
	INTEGER*4	SYMBOL_SIZE (NUMTYPES)	! Graphic symbol (static)
        COMMON / KINSYM / KINSYM, SYMBOL_SIZE

	REAL*4
     *                    SYSLEN  ( NUMTYPES ),
     *                    SYSWID  ( NUMTYPES ),
     *                    SYSHGT  ( NUMTYPES )

	COMMON / SYSLEN / SYSLEN, SYSWID, SYSHGT

	INTEGER*4	KGRFSYM (3,NUMTYPES)	! Graphic symbol (dynamic)
        COMMON / KGRFSYM / KGRFSYM

	INTEGER*4	KGRFSYM_REC (NUMTYPES)! the graphical symbol
						! to use when the icon is
						! recognised but not
						! identified
        COMMON / KGRFSYM_REC / KGRFSYM_REC

	INTEGER*4	NMIN  (NUMTYPES)	! Distance between elements
						!   (meters)
        COMMON / NMIN / NMIN

	BYTE 	IPST  (NUMTYPES)	! Height of system for LOS
						!  purposes (meters)
        COMMON / IPST / IPST

	INTEGER*4	MAXVIS (NUMTYPES)	! Maximum distance to let system
						!  look for detections (Km)
	INTEGER*4	MAXVIS_DND(NUMTYPES, 3)

        COMMON / MAXVIS / 
     *		MAXVIS,
     *		MAXVIS_DND

	REAL*4	PRIMRNG (NUMTYPES)	! Max effective range of
						!  system's primary weapon (Km)
        COMMON / PRIMRNG / PRIMRNG

	BYTE	KAN_DETECT_SENSOR(NUMTYPES, NUM_ACTIVE_TYPES)

	BYTE	KSENSTYP(2,NUMTYPES)		! Sensor Type
        COMMON / KSENSTYP / KSENSTYP, KAN_DETECT_SENSOR

	BYTE	KSENSNUM(NUMSENS,NUMTYPES)! all sensors on system
        COMMON / KSENSNUM / KSENSNUM

	INTEGER*4	ISARTYPE(NUMTYPES) ! if zero, then no sar, if >0
					! then points to index of
					! SAR_TYPE_LIST in GLOBSAR
        COMMON / ISARTYPE / ISARTYPE

	REAL	FOVMIN(NUMTYPES)	! min. field of view on sys.
        COMMON / FOVMIN / FOVMIN

	REAL	FOVMAX(NUMTYPES)	! max. field of view on sys.
        COMMON / FOVMAX / FOVMAX

	REAL	FOVPRIMARY(NUMTYPES)	! max. field of view on sys.
        COMMON / FOVPRIMARY / FOVPRIMARY

	BYTE	DESGNTR(NUMTYPES)		! Designators:
							!   1 = G1/G2
        COMMON / DESGNTR / DESGNTR

	BYTE 	ELECWAR(NUMTYPES)		!
	BYTE	SYSTEM_IFF(NUMTYPES) ! does this system type have IFF capability
        COMMON / ELECWAR / ELECWAR, SYSTEM_IFF

	BYTE	KMINDISP(NUMTYPES)		! Dispenser mechanism:
							! 0 = cant deploy mines
							! > 0 = panel type created
	BYTE	MINE_DISPENSER(NUMTYPES,3)
	REAL	MINE_DELAY(NUMTYPES,3)		! how long to delay after deploying minefield
	INTEGER*4	MINEFIELD_LOAD(NUMTYPES,3) ! how many minefields can this thing create before reloading

        COMMON / KMINDISP / KMINDISP, MINE_DISPENSER, MINE_DELAY, MINEFIELD_LOAD

	BYTE	BRIDGE_LAYER(NUMTYPES)	! can lay a bridge 0 = no, >0 = bridge type

	BYTE	ENGNEER(NUMTYPES)		! Engineer type:
							! 0 = none
							! 1 = TANK
							! 2 = AVLB
							! 3 = CEV
        COMMON / ENGNEER /
     *		ENGNEER,
     *		BRIDGE_LAYER

	BYTE 	FIRERS(NUMTYPES)		! 1 = Direct fire
							! 2 = Artillery
        COMMON / FIRERS / FIRERS

	BYTE	FLYERS(NUMTYPES)		! 1 - 15  = Flyer type
        COMMON / FLYERS / FLYERS

	BYTE	LOGSTCS(NUMTYPES)		! Logistics capability:
							!    1 = ASP/FARRP
        COMMON / LOGSTCS / LOGSTCS

	INTEGER*2	KMEDIC(NUMTYPES)		! 0= no, 1 = yes
        COMMON / KMEDIC / KMEDIC

	BYTE 	MOVERS(NUMTYPES)		! 1 = wheeled
							! 2 = tracked
							! 3 = footed
        COMMON / MOVERS / MOVERS

	BYTE	RADARS(NUMTYPES)		! Radar Type
        COMMON / RADARS / RADARS

	BYTE	REPAIRS(NUMTYPES)

        COMMON / REPAIRS / REPAIRS

	INTEGER*4	SMOKE_TYPE(NUMTYPES)		! smoke capability
							!     1 = exhaust
							!     2 = projectiles
							!     3 = both

	INTEGER*4	SMOKE_GRENADE_LOAD (NUMTYPES)
	INTEGER*4	SMOKE_GRENADE_STORE(NUMTYPES)

        COMMON / SMOKE_TYPE / SMOKE_TYPE, SMOKE_GRENADE_LOAD, SMOKE_GRENADE_STORE

	BYTE	SURVEIL(NUMTYPES)		! 1 = chemical detector
							! 2 = television
							! 3 = listener
        COMMON / SURVEIL / SURVEIL

	BYTE	SWIMMRS(NUMTYPES)		! 0 = can't swim
							! 1 = swims
	REAL	FORDING_DEPTH(NUMTYPES)	! fording depth (m) for this system type
        COMMON / SWIMMRS / SWIMMRS, FORDING_DEPTH

	BYTE	KANHOST(NUMTYPES)		! 0 = can't be a host
							! 1 = passenger can't
							!     detect targets
							! 2 = passenger can
							!     detect targets
        COMMON / KANHOST / KANHOST

	BYTE	KFIRPRI(NUMTYPES,NUMTYPES) ! Target Priority:
							! (firer-system,
							!  target-system,
							!  firer-side, target
							! side)

	BYTE	DETECT_PRIORITY (NUMTYPES, NUMTYPES)

        COMMON / KFIRPRI / KFIRPRI, DETECT_PRIORITY

	INTEGER*4	KSYSWGT(NUMTYPES)             ! Weight in lbs loaded
                                                        ! with fuel, crew, ammo.
        COMMON / KSYSWGT / KSYSWGT

	INTEGER*4	KSYSVOL(NUMTYPES)             ! Volume in CuFt loaded
                                                        ! with fuel, crew, ammo.
        COMMON / KSYSVOL / KSYSVOL

	INTEGER*4	KCRYWGT(NUMTYPES)             ! Additional Weight Sys.
                                                        ! Can carry after loaded
                                                        ! with fuel, crew, ammo.
        COMMON / KCRYWGT / KCRYWGT

	INTEGER*4	KCRYVOL(NUMTYPES)		! Additional Volume Sys.
							! Can carry after loaded
							! with fuel, crew, ammo.
        COMMON / KCRYVOL / KCRYVOL

	INTEGER*4	MOVEDE(NUMTYPES)		! movers using EDE data

        COMMON / MOVEDE / MOVEDE

	BYTE	EDETYP(NUMTYPES)		! pointer to EDE speed
							! array
        COMMON / EDETYP / EDETYP

	REAL	SIZT  (NUMTYPES)	! Detection dimension (meters)
        COMMON / SIZT / SIZT

	REAL	CRAWLSIZE(NUMTYPES)
	REAL	PARTIALSIZE(NUMTYPES)
	REAL	DEFILADESIZE(NUMTYPES)
	REAL	PITSIZE(NUMTYPES)

	REAL	POSTURE_HEIGHT(NUMTYPES, 0:NUMDEFL) ! height of system for detection purposes
	REAL	TARGET_HEIGHT(NUMTYPES, 0:NUMDEFL)  ! height of system when trying to detect

	COMMON / HEIGHT / POSTURE_HEIGHT, TARGET_HEIGHT

	REAL*4		SYSTEM_RECOG(NUMTYPES)
	REAL*4		SYSTEM_IDENT(NUMTYPES)

        COMMON / CRAWLSIZE / CRAWLSIZE, PARTIALSIZE, DEFILADESIZE, PITSIZE,
     *		SYSTEM_RECOG, SYSTEM_IDENT

	REAL	CRAWLSPEED(NUMTYPES)
        COMMON / CRAWLSPEED / CRAWLSPEED

	REAL	TRNSPD(NUMTYPES, NUMTRRNTYPES ) ! The max speed of a
							! unit in given trrn.
        COMMON / TRNSPD / TRNSPD

	REAL	TRNCONT(NUMTYPES, NUMTRRNTYPES )
			! The optical contrast pf this system
			! in a terrain type
        COMMON / TRNCONT / TRNCONT

	REAL	SUPPHFCT(NUMTYPES)	! The effect on a systems Ph of
						! suppressed
        COMMON / SUPPHFCT / SUPPHFCT

	REAL	SUPPDFCT(NUMTYPES)
        COMMON / SUPPDFCT / SUPPDFCT

	REAL	SUPMVFCT(NUMTYPES)
        COMMON / SUPMVFCT / SUPMVFCT

	REAL	SWIMSPEED(NUMTYPES)
        COMMON / SWIMSPEED / SWIMSPEED

	REAL	SLOPE_DATA (NUMTYPES, 3)
	COMMON / SLOPE_DATA / SLOPE_DATA	! stores slope effects on speed for system type
	
		INTEGER*4	SYS_POINT_DEFENCE(NUMTYPES)
	INTEGER*4	SYS_POINT_DEFENCE_ROUNDS(NUMTYPES)
	INTEGER*4	SYS_POINT_DEFENCE_STORED(NUMTYPES)

	INTEGER*4	SYS_ARMOUR(NUMTYPES)
	INTEGER*4	SYS_ARMOUR_PANELS(NUMTYPES)

	COMMON / SYS_POINT_DEFENCE /
     *		SYS_POINT_DEFENCE,
     *		SYS_POINT_DEFENCE_ROUNDS,
     *		SYS_POINT_DEFENCE_STORED,
     *		SYS_ARMOUR,
     *		SYS_ARMOUR_PANELS

	STRUCTURE / STRUC_DESIGNATOR /
	  INTEGER*4	TARGET_UNIT
	  INTEGER*4	TARGET_SIDE
	  INTEGER*4	FIRER_UNIT
	  INTEGER*4	FIRER_SIDE
	  INTEGER*4	ROUNDS
	END STRUCTURE



	RECORD / STRUC_DESIGNATOR / DESIGNATOR_LIST ( NUMDESIGNATORS)

	COMMON / DESIGNATORS /
     *		DESIGNATOR_LIST

	INTEGER*4	EMITTER_LIST(NUM_ACTIVE_SENSORS) ! list of icons that have active sensors
	INTEGER*4	RECEIVER_LIST(NUM_ACTIVE_SENSORS) ! list of icons able to detect active sensors

	INTEGER*4	NUM_EMITTERS
	INTEGER*4	NUM_RECEIVERS

	COMMON / ACTIVE_SENSOR_BLOCK/
     *		EMITTER_LIST,
     *		RECEIVER_LIST,
     *		NUM_EMITTERS,
     *		NUM_RECEIVERS



	INTEGER*4	OBST_CLEAR_TIME(NUMTYPES, MAX_OBST_TYPES)
	INTEGER*4	OBST_CROSS_TIME(NUMTYPES, MAX_OBST_TYPES)
	REAL*4		OBST_PMOB(NUMTYPES, MAX_OBST_TYPES)


	COMMON / OBSTACLE_DATA /
     *		OBST_CLEAR_TIME, OBST_CROSS_TIME, OBST_PMOB

	INTEGER*4	MAX_PITS
	PARAMETER	(MAX_PITS = 5000)

	INTEGER*4	PIT_TYPE(MAX_PITS)
	REAL*4		PIT_X(MAX_PITS)
	REAL*4		PIT_Y(MAX_PITS)
	INTEGER*4	PIT_FORCE(MAX_PITS)
	INTEGER*4	PIT_DETECTED(MAX_PITS,MAXFORCES)
	INTEGER*4	PIT_OCCUPANT(MAX_PITS)
	INTEGER*4	NUM_PITS
	INTEGER*4	IN_PIT (NUMUNITS, NUMSIDES)
	INTEGER*4	INITIAL_PITS(IPIT_VEHICLE, MAXFORCES)

	COMMON / PIT_DATA /
     *		PIT_TYPE, PIT_X, PIT_Y, PIT_FORCE, PIT_DETECTED, PIT_OCCUPANT,
     *		NUM_PITS, IN_PIT, INITIAL_PITS
	 */
	
}
