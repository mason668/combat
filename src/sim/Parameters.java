package sim;

public class Parameters {
	
	/**
	 * The first thing Janus wants to know is the run type.
	 * <p>
	 * See Constants for options.
	 * <p>
	 * normal
	 * checkpoint
	 * branchpoint
	 * branchpoint with planning
	 * non interactive
	 * non interactive batch
	 */
	private int runType = Constants.RUN_TYPE_NORMAL;
	public int getRunType(){return runType;}
	public void setRunType(int mode){
		if (mode < Constants.RUN_TYPE_NORMAL) return;
		if (mode >= Constants.RUN_TYPES) return;
		runType = mode;
	}

	/*
	 * Screen 1 parameters
	 */
	
	//TODO not in interpreter yet
	private String scenarioName = "unnamed";
	public String getScenarioName(){return scenarioName.substring(0);}
	public void setScenarioName(String name){
		if (name == null) return;
		scenarioName = name.substring(0);
	}
	
	//TODO not in interpreter yet
	private String mapFileName = "";
	public String getMapFileName(){return mapFileName;}
	public void setMapFileName(String name){
		if (name == null) return;
		mapFileName = name.substring(0);
	}
	
	//enable on-line help
	// maplike display
	// run type - see above
	// TODO random seed
	
	private double startTime = 0;
	public double getStartTime(){return startTime;}
	public void setStartTime(double time){startTime = time;} // TODO format time
	
	private double endTime = Constants.NEVER;
	public double getEndTime(){return endTime;}
	public void setEndTime(double time){endTime = time;} // TODO format time
	
	// time to increment clock each time through the loop
	private double epoch = 0.1; // seconds
	public double getIncrementAmount(){return epoch;}
	public void setIncrementAmount (double time){
		if (time > 0.0){
			epoch = time;
		}
	}
	
	/*
	 * Screen 2
	 */
	
	// table for workstation assignment
	// ws number, side, force, symbol size, window size, conwor mode
	
	/*
	 * Screen 3
	 * Suppression data
	 */
	
	/* direct fire probability coefficient
	 * arty lethal radius factor
	 * arty pk threshold
	 * suppr time soldier
	 * suppr time other
	 * suppr time arty
	 * defilade time
	 * he dust (on/ off)
	 * group speed - blue/ red
	 * step size for LOS (100.0m)
	 * 
	 */
	
	private double defiladeTime = 60.0; // seconds defltime
	public double getDefialdeTime(){return defiladeTime;}
	public void setDefiladeTime(double d){defiladeTime = d;}

	/*
	 * Screen 4
	 * Parameters
	 */

	/**
	 * Radar update time (Secs)
	 */
	private double radarUpdateTime = 2.0; // globunits.rdrupdint
	public double getRadarUpdateTime(){return radarUpdateTime;}
	public void setRadarUpdateTime(double time){
		if (time <= 0.0) return;
		radarUpdateTime = time;
		// should there be a max?
	}
	
	// TODO graphic update interval 2 sec

	/**
	 * Movement cycle time
	 */
	private double movementCycleTime = 5.0; // 5 secs dstmove
	public double getMovementCycleTime(){return movementCycleTime;}
	public void setMovementCycleTime(double time){movementCycleTime = time;}
	
	/**
	 * Direct fire cycle time
	 */
	private double directFireCycleTime = 5.0; // 5 secs dstfire
	public double getDirectFireCycleTime(){return directFireCycleTime;}
	public void setDirectFireCycleTime(double time){directFireCycleTime = time;}
	
	/**
	 * Casualty update time
	 */
	private double casualtyCycleTime = 5.0; // 5 secs dstcas
	public double getCasualtyCycleTime(){return casualtyCycleTime;}
	public void setCasualtyCycleTime(double time){casualtyCycleTime = time;}
	
	// * TODO move recording distance 0.5km
	
	/**
	 * Outer dismount radius.
	 */
	private double rangeDismountOuter = 0.2; //km
	public double getRangeDismountOuter(){return rangeDismountOuter;}
	public void setRangeDismountOuter(double range){
		if (range <=0) return;
		rangeDismountOuter = range;
	}
	
	/**
	 * Inner dismount radius.
	 */
	private double rangeDismountInner = 0.005; //km
	public double getRangeDismountInner(){return rangeDismountInner;}
	public void setRangeDismountInner(double range){
		if (range <=0) return;
		rangeDismountInner = range;
	}

	/**
	 * Outer mount radius.
	 */
	private double rangeMountOuter = 0.2; //km
	public double getRangeMountOuter(){return rangeMountOuter;}
	public void setRangeMountOuter(double range){
		if (range <=0) return;
		rangeMountOuter = range;
	}
	
	/**
	 * Inner mount radius.
	 */
	private double rangeMountInner = 0.005; //km
	public double getRangeMountInner(){return rangeMountInner;}
	public void setRangeMountInner(double range){
		if (range <=0) return;
		rangeMountInner = range;
	}

	/**
	 * Outer pit mount radius
	 */
	private double rangePitOuter = 0.2; // km;
	public double getRangePitOuter(){return rangePitOuter;}
	public void setRangePitOuter(double range){
		if (range <=0) return;
		rangePitOuter = range;
	}

	/**
	 * Inner pit mount radius
	 */
	private double rangePitInner = 0.005; // km;
	public double getRangePitInner(){return rangePitInner;}
	public void setRangePitInner(double range){
		if (range <=0) return;
		rangePitInner = range;
	}

	/*
	 * graphics update interval )2 sec)
	 * upload distance 0.2km
	 * avlb distance 0.3km
	 * obstacle detection 0.1min
	 * report period (statwt) blank (ie none?)
	 * auto plan save (min)
	 * day/ night/ dusk (1/2/3) 1
	 * avlb wait time 20 sec
	 * artillery tight radius 0.2km
	 * arty loose radius 0.4km
	 * arty div radius 0.3km
	 * arty adjust radius 0.05km
	 * secondary fov ratio 0.2
	 * pd factor for running 0.25 - should be system based?
	 * infatry pit dimensions 1.0
	 * vehicle pit dimension 1.0
	 * 
	 */

	/*
	 * Screen 5
	 * obstacles
	 * smoke pots 1 = hc, 2= wp, 3 = bs, 4 = not used, 5 = fo
	 * vehicle exhaust
	 * vehicle projectiles
	 * should all be system based
	 */
	
	/*
	 * Screen 6
	 * sar targets
	 * number of innocuous targets
	 */
	
	/*
	 * screen 7
	 * conwor setup
	 * table, for each of 9 conwor modes
	 * for each force, what colour 0 = no display
	 * Should be force attribute
	 * 
	 */
	
	/*
	 * Screen 8
	 * on/ off params
	 * Friendly engagements
	 * Direct fire area effacts
	 * passenger survivability
	 * display destroyed icons
	 * casualty evac
	 */

	/**
	 * Determine if detection of friendly forces is enabled.
	 */
	private int doSeeOwn = Constants.DETECT_FRIENDS_WHEN_SEEN;
	public int getDoSeeOwn(){return doSeeOwn;}
	public void setDoSeeOwn(int mode){
		if (mode < Constants.DETECT_FRIENDS_NEVER) return;
		if (mode > Constants.DETECT_FRIENDS_ALWAYS) return;
		doSeeOwn = mode;
	}
	
	/*
	 * screen 9
	 * zoom scales
	 * for each of 1-13 scales, size of map view
	 * 
	 */
	
	/*
	 * screen under development
	 * menu debugging
	 * hetero aggregation
	 * vehicle repair/ recovery
	 * alternate menu
	 * roe options
	 * rof options
	 * eng functions
	 * pits
	 * run function
	 */

	/**
	 * Mobility and firepower kills
	 */
	private boolean partialKills = false; // dopartkill
	public boolean getPartialKills(){return partialKills;}
	public void setPartialKills(boolean b){partialKills = b;}

	/*
	 * Other run time parameters
	 */
	
	/**
	 * Real time synchronisation
	 */
	private boolean realTimeSynch = true;
	public boolean getRealTimeSynch(){return realTimeSynch;}
	public void setRealTimeSynch(boolean setting){realTimeSynch = setting;}
	
	/**
	 * Real time ratio.
	 */
	private double realTimeRatio = 1.0;
	public double getRealTimeRatio(){return realTimeRatio;}
	public void setRealTimeRatio(double ratio){
		if (ratio <= 0.01) return;
		if (ratio > 20.0) return;
		realTimeRatio = ratio;
	}
	
	/* from jparamrd
	READ (IFILESCRCH)
     *		TSUPRS1,
     *		TSUPRS2,
     *		SUPRSCOEF,
     *		ARTSUPTIM,
     *		SUPRADFCTR,
     *		PKSUPTHRSH,
     *		KSOIL,
     *		GROUPSPEED,
     *		INFSPEED,
     *		UNRECPH
     *
     *
     *		GUPINT,
     *		RPERIOD,
     *		DOSEEOWN,
     *		DOSHOOTOWN,
     *		DO_DF_SUPPR,
     *		MAXTRGTS,
     *		DOSEEDEAD,
     *		DOCASTYPES,
     *		DOSMARTFMN,
     *		IHFLIMIT
     *		DODEPLOY_MINES
     *		DTOBS
     *		DOHETERO
     *
     *		NUM_FALSE_TARGETS,
     *		REMOVE_DELAY,
     *		SHOW_DELAY
     *
     *		FORCE_COLOUR
     *
     *		NET_CONNECT,
     *		NET_MOVE,
     *		NET_DETECT,
     *		NET_ART_IMPACT,
     *		NET_ARTILLERY,
     *		NET_DF_IMPACT,
     *		NET_STATUSRPT,
     *		NET_HULK
     *
     *		DO_ARTY_AGGR
     *
     *		DIST_UPDATE_MOVE,
     *		FREQ_PLANSAVE
     *
     *		RNGMOUNT,
     *		RNGDISMOUNT,
     *		RNGUPLOAD,
     *		DSTRADAR
     *
     *		RNGBRIDGE,
     *		ARTY_LOOSE_RADIUS,
     *		ARTY_TIGHT_RADIUS,
     *		ARTY_ADJUST_RADIUS,
     *		LOS_STEP_SIZE
     *
     *		SECONDARY_FOV_RATIO,
     *		DOOPTIONS,
     *		DEBUGMENU,
     *		ZSCALE

	READ (IFILESCRCH, ERR=800)
     *		DO_ALTMENU, DO_ROF, DO_ROE, DO_ENG, DO_PIT, DO_RUN

	READ (IFILESCRCH, ERR=800)
     *		RNGPIT, RNGMOUNT_INNER, RNGDISMOUNT_INNER, RNGPIT_INNER

	READ (IFILESCRCH, ERR=800)
     *		BRIDGE_WAIT_TIME, DOPASSKILLS, DAYNIGHT

	READ (IFILESCRCH, ERR=800)
     *		RUN_PD_FACTOR

	READ (IFILESCRCH, ERR=800)
     *		RUN_PD_FACTOR

	READ (IFILESCRCH, ERR=800)
     *		PIT_DET_DIMENSIONS
     *
     *
	 * 
	 */
	
	private double speedMOPP = 0.5;
	public double getSpeedMOPP(){return speedMOPP;}
	public void setSpeedMOPP(double d){
		if (d > 1.0) d = 1.0;
		if (d < 0.0) d = 0.0;
		speedMOPP = d;
	}
	
	private double weatherMove = 1.0;
	public double getWeatherMove(){return weatherMove;}
	public void setWeatherMove(double d){
		if (d > 1.0) d = 1.0;
		if (d < 0.0) d = 0.0;
		weatherMove = d;
	}
	
	private double checkPointFrequency = 0.0;
	public double getCheckPointFrequency(){return this.checkPointFrequency;}
	public void setCheckPointFrequency(double time){
		if (time < 0.0) return;
		this.checkPointFrequency = time;
	}
	
	public String list(){
		String s = "";
		s = s + "Scanrio Parameters";
		return s;
	}
	
	/*
	 * fromm trrnlib

	 * 	SUBROUTINE GET_HELP_FLAG ( IFLAG )

	include 'global.f'

	IFLAG = 0

	IF ( DOHELP_MENU ) IFLAG = 1

	RETURN
	END

	SUBROUTINE GET_HETERO_FLAG ( IFLAG )

	include 'global.f'

	IFLAG = 0

	IF ( DOHETERO .GT. 0) IFLAG = 1

	RETURN
	END

	SUBROUTINE GET_DF_SUPPR_FLAG ( IFLAG )

	include 'global.f'

	IFLAG = 0

	IF ( DO_DF_SUPPR ) IFLAG = 1

	RETURN
	END


	SUBROUTINE GET_MAP_FLAG ( IFLAG )

	include 'global.f'

	IFLAG = 0

	IF ( WHITE_MAP .GT. 0 ) IFLAG = 1

	RETURN
	END


	 */
	
}
