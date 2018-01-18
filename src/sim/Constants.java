package sim;

public class Constants {
	
	public static final double VERY_CLOSE = 0.0001;// use to test if things are close to one another (km)
	public static final double TWO_PI = Math.PI* 2;

	public static final int DEFILADE_STATES = 3;
	public static final int DEFILADE_EXPOSED = 0; 
	public static final int DEFILADE_PARTIAL = 1; 
	public static final int DEFILADE_FULL = 2; 
	public static final String DEFILADE_STATE[] ={
			"exposed","partial","full",
	};


	// TODO are these needed?
	public static final int ROE_VERY_FREE=0; // can shoot anyone not +ve friendly
	public static final int ROE_FREE=1; // can shoot anyone not +ve friendly or neutral
	public static final int ROE_VERY_LOOSE=2; // as loose, but can also target neutrals aiming
	public static final int ROE_LOOSE=3; // as tight, but can also shoot unknown aiming at this force
	public static final int ROE_TIGHT=4; // only shoot positive enemy
	public static final int ROE_VERY_TIGHT=5; // only +ve enemy and only if they are actually shooting
	public static final int ROE_HOLD=6; // will aim but not shoot
	public static final int ROE_DOWN=7; // will not aim
	public static final int ROE_CONCEALED=8; // do not appear to have weapons
	
	// see globunits
	public static final int ROF_LOW = 0;
	public static final int ROF_AVERAGE = 1;
	public static final int ROF_HIGH = 2;
	public static final String RATE_OF_FIRE[] = {"low", "average", "high"}; 
	
	public static final int OBSERVATION_LEVELS = 4;
	public static final int OBSERVATION_LEVEL_UNSEEN = 0;
	public static final int OBSERVATION_LEVEL_DETECTED = 1;
	public static final int OBSERVATION_LEVEL_RECOGNISED = 2;
	public static final int OBSERVATION_LEVEL_IDENTIFIED = 3;

	public static final String OBSERVATION_LEVEL[] = {"unseen", 
			"detected", "recognised", "identified"}; 

	public static final int HOSTILITY_UNKNOWN = 0;
	public static final int HOSTILITY_FRIEND = 1;
	public static final int HOSTILITY_NEUTRAL = 2;
	public static final int HOSTILITY_ENEMY = 3;
	public static final int HOSTILITY_UNSEEN = 4;

	public static final String HOSTILITY_LEVEL[] = {"unseen", 
			"friendly", "neutral", "enemy", "unknown"}; 
	
	// How many relationship types a force can have
	public static final int HOSTILITY_TYPES = 4;
	
	public static final int MOVER_TYPES = 9;
	public static final int MOVER_NONE = 0;
	public static final int MOVER_FOOT = 1;
	public static final int MOVER_WHEEL = 2;
	public static final int MOVER_TRACK = 3;
	public static final int MOVER_TOWED = 4;
	public static final int MOVER_SHIP = 5;
	public static final int MOVER_BOAT = 6;
	public static final int MOVER_CRAFT = 7;
	public static final int MOVER_HOVER = 8;
	public static final String MOVER_TYPE[] = {
			"static", "foot", "wheel", "track", "towed", 
			"ship", "boat", "craft", "hover"
	};
	
	public static final double NEVER = 60*60*24*99; // 99 days

	public static final int BRIDGE_STATES = 4;
	public static final int BRIDGE_RECOVERED = 0;
	public static final int BRIDGE_RECOVERING = 1;
	public static final int BRIDGE_DEPLOYED = 2;
	public static final int BRIDGE_DEPLOYING = 3;
	public static final String[] BRIDGE_STATE = {
			"recovered", "recovering","deployed", "deploying"
	};
	
	public static final int CBR_PACKED = 0;
	public static final int CBR_SETUP = 1;
	public static final int CBR_ON = 2;
	public static final String CBR_STATUS[] ={
			"packed", "setup-off", "setup-on"
	};
	
	public static final int MOVE_NORMAL = 0;
	public static final int MOVE_GROUP = 1;
	public static final int MOVE_SLOW = 2;
	public static final int MOVE_CRAWL = 3;
	public static final int MOVE_ASSAULT = 4;
	public static final int MOVE_REVERSE = 5;
	public static final int MOVE_RUN = 6;
	public static final int MOVE_RUSH = 7;
	public static final int MOVE_CAUTIOUS = 8;
	public static final int MOVE_ADVANCE = 9;
	public static final String MOVE_MODE[] ={
			"normal","group","slow","crawl","assault",
			"reverse","run","rush", "cautious","advance"
	};
	
	public static final int VIEW_NORMAL = 0;
	public static final int VIEW_OFFSET = 1;
	public static final int VIEW_POINT = 2;
	public static final String[] VIEW_MODE = {
			"normal", "offset", "point",
	};
	
	public static final int FLY_AGL = 0;
	public static final int FLY_AMSL = 1;
	public static final String[] FLIGHT_MODE = {
			"agl", "amsl",
	};
	
	public static final int MINE_CLEARING_MODES = 5;
	public static final int MINE_CLEAR_BULL = 0;
	public static final int MINE_CLEAR_PUSH = 1;
	public static final int MINE_CLEAR_INFANTRY = 2;
	public static final int MINE_CLEAR_PLOUGH = 3;
	public static final int MINE_CLEAR_ROLLER = 4;
	public static final int MINE_CLEAR_CHARGE = 5;
	public static final String[] MINE_CLEAR_MODE = {
			"bull", "push", "infantry", "plough", "roller", "line charge",
	};
	
	public static final int DETECT_FRIENDS_NEVER = 0;
	public static final int DETECT_FRIENDS_WHEN_SEEN = 1;
	public static final int DETECT_FRIENDS_ALWAYS = 2;
	public static final String DETECT_FRIENDS[] = {
			"never", "when seen", "always",
	};
	
	public static final int RUN_TYPES = 6;
	public static final int RUN_TYPE_NORMAL = 0;
	public static final int RUN_TYPE_CHECKPOINT = 1;
	public static final int RUN_TYPE_BRANCHPOINT = 2;
	public static final int RUN_TYPE_BRANCH_WITH_PLANNING = 3;
	public static final int RUN_TYPE_NON_INTERACTIVE = 4;
	public static final int RUN_TYPE_NON_INTERACTIVE_BATCH = 5;
	public static final String[] RUN_TYPE = {
		"normal", "checkpoint","branchpoint","branchpoint with planning",
		"non interactive", "non interactive batch"
	};
	
	public static final int MAX_TRIGGERS = 99; // see globunits
	public static final int MAX_OBSTACLE_TYPES = 5; // see globunits
	public static final int MAX_CASUALTY_TYPES = 5; // see globunits
	
	public static final int INOPERATIVE_STATES = 5;
	public static final int INOPERATIVE_ATROPINE = 1;
	public static final int INOPERATIVE_LOSS_OF_CONTROL = 2;
	public static final int INOPERATIVE_LETHAL_DOSE = 3;
	public static final int INOPERATIVE_DETECTOR_SET = 4;
	public static final int INOPERATIVE_HEAT = 5;
	public static final String[] INOPERATIVE_REASON = {
			"operative", "atropine", "loss of control",
			"lethal dose", "detector set", "heat"
	};
	/* from globunits
	BYTE	KINOPSTAT (NUMUNITS,NUMSIDES)	! The inoperative status of
						! this unit:
						!    11 = due to atropine
						!    12 = loss of control/chem
						!    13 = lethal chem dose
						!    14 = detector already set
						!    15 = loss of control/heat
	 * 
	 */
	public static final int MAX_FUEL_TYPES = 4;
	public static final int NO_FUEL = 0;
	public static final int PETROL = 1;
	public static final int DIESEL = 2;
	public static final int AVGAS = 3;
	public static final String[] FUEL_TYPE = {
			"none", "petrol","diesel","avgas"
	};
	
	public static final int MAX_CARRIER_TYPES = 3;
	public static final int NO_PASSENGERS = 0;
	public static final int CAN_ACQUIRE = 1;
	public static final int CANT_ACQUIRE = 2;
	public static final String[] CARRIER_TYPE = {
			"no passengers", "can acquire", "can't acquire"
	};


}
