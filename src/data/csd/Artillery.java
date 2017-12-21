package data.csd;

import java.util.HashMap;

/**
 * Define class to represent artillery data.
 * See globartc.f
 *
 */
public class Artillery {
	
	public enum ArtilleryEnvironment {OPEN, WOOD, TOWN};
	public static final int NUMBER_OF_ROUND_TYPES = 15;// numproj
	public enum ArtilleryRoundType {HE, ICM, RAP, FASCAM, FLECHET, }; // numproj 15
	
	private HashMap<ArtilleryRoundType, ArtilleryProjectile> roundData = 
			new HashMap<ArtilleryRoundType, ArtilleryProjectile>();
	
	/*
	 * Ammunition reliability (probability 0.0= 1.0) in each environment type
	 */
	private HashMap<ArtilleryEnvironment, Double> reliability =
			new HashMap<ArtilleryEnvironment, Double>(); // camunrel
	private HashMap<ArtilleryEnvironment, Double> submunitionReliability =
			new HashMap<ArtilleryEnvironment, Double>(); // csmunrel
	
	/*
	 * Number of tubes per system
	 */
	private int tubes = 1; // ktubes
	
	/*
	 * Calibre in mm
	 */
	private int calibre = 120; // kartcalc

	/*
	 * Time (secs) to setup system for firing
	 */
	private int setupTime = 120; // csetuptim
	
	/*
	 * Time (sec) to packup system ready to move
	 */
	private int packupTime = 120; // ctdwntim
	
	/*
	 * Number of bomblets per round (ICM)
	 */
	private int bomblets = 1; // kbomblc
	
	/*
	 * Number of FASCAM mines per round
	 */
	private int mines=1; // kfasrndc
	
	/*
	 * 
        REAL*4          CEFFICSL(NUMCSDINDF)
        REAL*4          CEFFICIN(NUMCSDINDF)
     *            CEFFICSL,       ! Slope for ICM radius of effects
     *            CEFFICIN,       ! Intercept for ICM radius of effects
        REAL*4          CRAPLAFC(NUMCSDINDF)
        REAL*4          CWPHEFAC(NUMCSDINDF)
        REAL*4          CFLICFAC(NUMCSDINDF)
     *            CRAPLAFC,       ! RAP to HE lethal area factor
     *            CWPHEFAC,       ! White Phos. to HE lethal area factor
     *            CFLICFAC,       ! Flechette to HE lethal area factor
        REAL*4          CANGLTHL(NUMARTANGS,NUMCSDINDF)
        REAL*4          CARTHELTH(NUMARTANGS,NUMARTVULS,NUMARTENVS,
     *                                                      NUMCSDINDF)
        REAL*4          CARTICLTH(NUMARTVULS,NUMARTENVS,NUMCSDINDF)
     *            CARTHELTH,      ! HE lethality areas
     *            CARTICLTH,      ! ICM lethality areas
        REAL*4          CARTHEBR(NUMCSDINDF)
     *            CARTHEBR,       ! HE blast radius for Building Rubbling
        REAL*4          CHEPKIL(5,NUMARTVULS)
     *            CHEPKIL,        ! HE probability of kill, First index:
                                  !       Vehicle         Footed
                                  !     1  passenger      return to duty
                                  !     2  mobility       wounded in action
                                  !     3  firepower      litter urgent
                                  !     4  mob&fpr        litter priority
                                  !     5  catastrophic  catastrophic
        REAL*4          CICMPKIL(5,NUMARTVULS)
     *            CICMPKIL,       ! ICM probability of kill (see HE)
        REAL*4          CHEPCREW(4,NUMARTVULS)
     *            CHEPCREW,       ! HE probability of crew kill, First index:
                                  !     1  mobility kill, mechanical damage
                                  !     2  mobility kill, crew kill
                                  !     3  fire power kill, mechanical damage
                                  !     4  fire power kill, crew kill
        REAL*4          CICMPCREW(4,NUMARTVULS)
     *            CICMPCREW,      ! ICM probability of crew kill (see HE)



        BYTE            KATRALGC(NUMARTVULS,NUMCSDINDF)
     *            KATRALGC,       ! HE attrition algorithm:
                                  !     1 = COOKIE CUTTER
                                  !     2 = CARLTON DAMAGE
        BYTE            KVULCATC(2,NUMCSDTYPES)
					! arty system 
        BYTE            ARTILLERY_VULNERABILITYC(10,NUMCSDTYPES)

        COMMON  / GLOBARTC1 /
 
     *            CANGLTHL,       ! Angles of fall for lethality data
     *            KVULCATC,       ! Vulnerability levels.
                                  !     First index: 1 = exposed
                                  !                  2 = defilade.
     *		ARTILLERY_VULNERABILITYC
	 */

}
