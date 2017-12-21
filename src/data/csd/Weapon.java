package data.csd;

import java.util.HashMap;
import java.util.Vector;

import data.managers.Identifiable;
import interpreterOLD.FileLoader;
import interpreterOLD.InterpreterInterface;
import sim.Constants;

public class Weapon {
	public static void main(String[] args){
		System.out.println("weapon");
	}
	
	public Weapon (String s){
		name = s.substring(0);
	}
	
	private String name = "test_weapon";
	public String getName(){return name.substring(0);}
	public void setName(String s){name = s.substring(0);}
	
	// round velocity in m/s
	private double roundVelocity = 200.0; // globwpnc.croundvel
	
	/*
     *                  CTIMEFIR  (NUMCSDWPNS),
     *                  CTIMLOAD  (NUMCSDWPNS),
     *                  CTIMELAY  (NUMCSDWPNS),
     *                  CSSKPMIN  (NUMCSDWPNS),
     *			WPNSUPTIMC (NUMCSDWPNS,NUMARTVULS),
     *			KILLRADC (NUMCSDWPNS,NUMARTVULS),
     *			SUPPRADC (NUMCSDWPNS,NUMARTVULS),
     *			PIT_PHC (NUMCSDWPNS,NUMARTVULS),
     *			PIT_PKC (NUMCSDWPNS,NUMARTVULS)

	*/

	/*
	private HashMap<SheafSize, Double> SheafX = 
			new HashMap<SheafSize, Double>(); // globwpnc.cdf_sheaf_x
	private HashMap<SheafSize, Double> SheafY = 
			new HashMap<SheafSize, Double>(); // globwpnc.cdf_sheaf_y
			*/
	private static final double sec3 = 3.0;
	private static final double sec5 = 5.0;

	private double[] preFireTime = {sec3, sec3, sec3};
	public double getPreFireTime(int rof){
		if (rof < 0) return -1;
		if (rof > Constants.ROF_HIGH) return -1;
		return preFireTime[rof];
	}
	private double[] postFireTime = {0, 0, 0};
	private double[] newTargetTime = {sec5, sec5, sec5};
	public double getNewTargetTime(int rof){
		if (rof < 0) return -1;
		if (rof > Constants.ROF_HIGH) return -1;
		return newTargetTime[rof];
	}
	
	private double maxRange= 10.0; // globwpnc.weapon_rangec
	public void setMaxRange(double d){maxRange = d;}
	public double getMaxRange(){return maxRange;}
	
	private double minAltitude = 0.0; // globwpnc.weapon_min_altitude
	private double maxAltitude = 5000.0; // globwpnc.weapon_max_altitude
	private int pullsPerReload = 10; // globwpnc.kpullsc
	private int roundsPerPull = 3; // globwpnc.kroundsc
	private boolean canFireAndMove = true; // globwpnc.kfirmovc
	
	// probability this weapon will trigger an active defence system
	private HashMap<ActiveDefenceType,Double> activeDefenceTriggerProbability =
			new HashMap<ActiveDefenceType, Double>(); // globwpnc.point_defence_triggerc
	// probability, having triggered the actove defence, this weapon will be intercepted
	private HashMap<ActiveDefenceType,Double> activeDefenceEffect =
			new HashMap<ActiveDefenceType, Double>(); // globwpnc.point_defence_effectc
	// probability this weapon will trigger reactive armour
	private HashMap<ArmourType,Double> armourTriggerProbability =
			new HashMap<ArmourType, Double>(); // globwpnc.armour_triggerc
	// probability, given triggering, this weapon will be defeated by reactive armour
	private HashMap<ArmourType,Double> armourEffect =
			new HashMap<ArmourType, Double>(); // globwpnc.armour_effectc
	// probability, given triggering, this weapon will cause the armour to be depleted
	private HashMap<ArmourType,Double> armourDestroyed =
			new HashMap<ArmourType, Double>(); // globwpnc.armour_destroyedc
	
	public static final int FIRE_ON_THE_MOVE_YES = 0;
	public static final int FIRE_ON_THE_MOVE_BEFORE_IMPACT = 1;
	public static final int FIRE_ON_THE_MOVE_AFTER_IMPACT = 2;
	public static final int FIRE_ON_THE_MOVE_SLOW = 3;
	public static final int FIRE_ON_THE_MOVE_POPUP = 4;
	public static final String fireOnTheMoveText[] = {"fire while moving", 
			"stop to fire", "stop until impact", "slow to fire", "popup to fire"};
	private int fireOnTheMove  = 0; // kfirmovc
	public int getFireOnTheMove(){return fireOnTheMove;}
	public void setFireOnTheMove(int i){
		if (i < 0) return;
		if (i> FIRE_ON_THE_MOVE_POPUP) return;
		fireOnTheMove = i;
	}
	
	/*

     *                  KRITALTC (NUMCSDWPNS)
     *                  KGUIDEC   (NUMCSDWPNS),
     *                  KWPNSNSRC (NUMCSDWPNS)

	!					the way this works is that LOS is calculated. If LOS > max, then PH is not affected
						! if LOS is <= min, the PH = 0.0
						! if LOS is < MAX > MIN then the value is linearly interpolated between factor and zero
						! and then multiplied x PH
	REAL*4		LOS_MAX_THRESHHOLDC (NUMCSDWPNS,2) ! LOS above which there is no effect
	REAL*4		LOS_MIN_THRESHHOLDC (NUMCSDWPNS,2) ! LOS below which PH is zero
	REAL*4		LOS_FACTORC (NUMCSDWPNS,2)	 ! PH factor to be applied when LOS is at MAX
	
	REAL*4		WIRE_MAX_THRESHHOLDC (NUMCSDWPNS,2)
	REAL*4		WIRE_MIN_THRESHHOLDC (NUMCSDWPNS,2)
	REAL*4		WIRE_FACTORC (NUMCSDWPNS,2)

     *		CSSKPMIN,           ! Minimum SSKP

     *		KGUIDEC,            ! GUIDANCE FLAG:
                                    !    0 = No guidance
                                    !    1 = Guidance required
                                    !    2 = Can't track thru smoke

     *		KRITALTC,           ! CRITCAL ALTITUDE ABOVE GROUND IN FEET

     *		KWPNSNSRC,           ! WEAPON SENSOR NUMBER

     *		WPNSUPTIMC,		! suppression time for weapon vs target type (secs)
     *		KILLRADC,		! kill radius for weapon vs target type (m)
     *		SUPPRADC,		! suppression radius for wpn vs target type (m)
     *		PIT_PHC, PIT_PKC,
     *		LOS_MAX_THRESHHOLDC, LOS_MIN_THRESHHOLDC, LOS_FACTORC, WIRE_MAX_THRESHHOLDC,
     *		WIRE_MIN_THRESHHOLDC, WIRE_FACTORC,
     *		CDF_SHEAF_X, CDF_SHEAF_Y, CDF_PRE_FIRE, CDF_POST_FIRE, CDF_NEW_TARGET

C ----- COMMON  / GLOBWPNC2 /
	*/
	private HashMap<String,PHTable> platformPHTableList =
			new HashMap<String, PHTable>(); // globwpnc.csdphc
	public void setPlatformPH(String platformName, PHTable phTable){
		platformPHTableList.put(platformName, phTable);
	}
	public PHTable getPlatformPH(String platformName){
		return platformPHTableList.get(platformName);
	}
	private HashMap<String,PKTable> platformPKTableList =
			new HashMap<String, PKTable>(); // globwpnc.csdpkc
	public void setPlatformPK(String platformName, PKTable pkTable){
		platformPKTableList.put(platformName, pkTable);
	}
	public PKTable getPlatformPK(String platformName){
		return platformPKTableList.get(platformName);
	}
	private HashMap<String,PHTable> passengerPHTableList =
			new HashMap<String, PHTable>(); // globwpnc.csdphpasc
	public void setPassengerPH(String platformName, PHTable phTable){
		passengerPHTableList.put(platformName, phTable);
	}
	public PHTable getPassengerPH(String platformName){
		return passengerPHTableList.get(platformName);
	}
	private HashMap<String,PKTable> passengerPKTableList =
			new HashMap<String, PKTable>(); // globwpnc.csdpkpasc
	public void setPassengerPK(String platformName, PKTable pkTable){
		passengerPKTableList.put(platformName, pkTable);
	}
	public PKTable getPassengerPK(String platformName){
		return passengerPKTableList.get(platformName);
	}
	
	/*

        INTEGER*2       CSDPKPASKILLC(NUMCSDWPNS,NUMCSDTYPES)

	COMMON  / GLOBWPNC4 /

     *		CSDPKPASKILLC             ! For each weapon vs each target, the CSD
                                    ! PH data set.

	 */
	
	private double moppFactor= 0.5;
	public double getMoppFactor(){return moppFactor;}

}
